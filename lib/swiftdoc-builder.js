var Twig = require('twig'),
    mkdirp = require('mkdirp'),
    fs = require('fs'),
    execsyncs = require("execsyncs"),
    grapher = require('./swiftdoc-grapher'),
    ext = require('./type-extensions'),
    markdown = require('markdown').markdown,
    PCRE = require('pcre').PCRE,
	path = require('path'),
    twig = Twig.twig;

var DEBUG = false;
var appRoot = path.join(__dirname, '../');
var svgLogo = fs.readFileSync(appRoot + 'data/svg-logo.txt', 'utf8');
var outputDir = '';
var urlPrefix = '';
var dotPath = '/usr/local/bin/dot';
var outputGraphs = true;

// ----------------------------------------------------------------------------
// Page titles and descriptions

var pageInfo = {
    home: {
        title: 'Home',
    },
    var_list: {
        title: 'Global Variables',
        description: 'Documentation for global variables in the Swift language.',
    },
    alias_list: {
        title: 'Type Aliases',
        description: 'Documentation for global type aliases in the Swift language.',
    },
    index_type: {
        title: 'Types',
        description: 'Documentation for all classes, structs and enums in the Swift language.',
    },
    index_protocol: {
        title: 'Protocols',
        description: 'Documentation for all protocols in the Swift language.',
    },
    index_operator: {
        title: 'Operators',
        description: 'Documentation for all operators in the Swift language.',
    },
    index_global: {
        title: 'Globals',
        description: 'Documentation for all global variables, type aliases, and functions in the Swift language.',
    },
    index_func: {
        title: 'Global Functions',
        description: 'Documentation for all top-level, global functions in the Swift language.',
    },
};

// ----------------------------------------------------------------------------
// Twig templates

var templates = {
    var_list       : Twig.twig({ path: appRoot + 'templates/var-list.html', async: false }),
    alias_list     : Twig.twig({ path: appRoot + 'templates/alias-list.html', async: false }),

    operator       : Twig.twig({ path: appRoot + 'templates/operator.html', async: false }),
    func           : Twig.twig({ path: appRoot + 'templates/func.html', async: false }),
    type           : Twig.twig({ path: appRoot + 'templates/type.html', async: false }),
                   
    index_func     : Twig.twig({ path: appRoot + 'templates/index-func.html', async: false }),
    index_global   : Twig.twig({ path: appRoot + 'templates/index-global.html', async: false }),
    index_home     : Twig.twig({ path: appRoot + 'templates/index-home.html', async: false }),
    index_operator : Twig.twig({ path: appRoot + 'templates/index-operator.html', async: false }),
    index_type     : Twig.twig({ path: appRoot + 'templates/index-type.html', async: false }),
};

// ----------------------------------------------------------------------------
// Twig filters

// returns true if item (can be a property or a method) is static/class
function isStatic(item) {
    return ((item['stat'] == 'static') || (item['stat'] == 'class') || (item['note'] == 'static') || (item['note'] == 'class'));
}

// ReStructuredText code blocks use fewer spaces than markdown - fix that
function convertCodeBlocks(value) {
    // if (value.indexOf('Instances of conforming types can be compared') == -1) return value;
    
    var codeBlockStart = new PCRE('::[ \\t]*$', PCRE.PCRE_EXTENDED | PCRE.PCRE_MULTILINE);
    var codeBlockEnd = new PCRE('^\\S', PCRE.PCRE_EXTENDED | PCRE.PCRE_MULTILINE);
    var match, endMatch;
    while (match = codeBlockStart.exec(value)) {
        if (endMatch = codeBlockEnd.exec(value, match[1])) {
            value = value.substring(0, match[0] + 1) + 
                    value.substring(match[1], endMatch[0]).replace(/^  /gm, '    ') +
                    value.substring(endMatch[0]);
        } else {
            value = value.substring(0, match[0] + 1) + 
                    value.substring(match[1]).replace(/^ /gm, '   ');
        }
    }
    return value;
}

// convert comment from quote-unquote-ReStructuredText to html
function convertComment(value) {
    value = value.replace(/\\\\ ?&#039;/g, '\'')             // weird characters
                .replace(/`([^`]+)\n?<(https?:\/\/[^>]+)>`_*/mgi, '[$1]($2)') // convert RST link syntax to MD
                .replace(/\.\. parsed-literal::?/g, '::')    // convert funky syntax to code block
                .replace(/\.\. ([^:\n]+)::/g, '**$1:**')     // incorrect format for inline header?
                .replace(/:param: ([^ ,]+)/g, '**$1**')      // parameter
                .replace(/:returns: /g, '**Returns:** ')     // return value
                .replace(/_,/g, ',')                         // Array has a weird internal link
                .replace(/\.\. _.+?:/g, '')

                // Swift 2.0 special notations
                
                .replace(/- parameter +([^ ,]+):/gi,         // parameter
                        '\r**`$1`:** ')     
                .replace(/- See ?Also: (.+)/gi,              // see also
                        '**See Also:** $1')     
                .replace(/- see ?also:/g,                    // see also
                        '**See Also:**\n\n')     
                .replace(/- ([^:\s]+):/g, '**$1:**')         // special notes                
                ;

    value = convertCodeBlocks(value);
    var html = markdown.toHTML(value);
    html = html
                /*
                .replace(/&amp;lt;/g, '&lt;')                // fix doubly escaped <
                .replace(/&amp;gt;/g, '&gt;')                // >
                .replace(/&amp;quot;/g, '&quot;')            // quotes
                .replace(/&amp;amp;/g, '&amp;')              // &
                */
                .replace(/&amp;#039;/g, '\'')                // '
                .replace(/<code>([^<]+?) *&lt;(http.+?)&gt;<\/code>_+/g, '<a href=\'$2\'>$1</a>') // links get screwed up
                .replace(/&lt;(http.+?)&gt;/g, '<a href=\'$1\'>$1</a>') // more linking
                .replace(/\s+<\/a>/g, '</a>')                // more linking
                .replace(/\\\\ /g, '')                       // get rid of extraneous double-backslashes
                .replace(/\\ /g, '')                         // and extraneous single-backslashes
                .replace(/<p>:<\/p>\n?\n?/, '')              // leftovers from '::' on own line
                .replace(/<pre><code>/g, '<pre><code class="language-swift">'); // add language class to pre/code blocks

    return html;
}

Twig.extendFilter('rst', convertComment);

Twig.extendFilter('withSpaceAfter', function(value) {
    return (value) ? value + ' ' : '';
});

Twig.extendFilter('static', function(values) {
    return values.filter(function(val) { 
        return isStatic(val);
    });
})

Twig.extendFilter('instance', function(values) {
    return values.filter(function(val) { 
        return !isStatic(val);
    });
})

Twig.extendFilter('addSoftBreaks', function(value) {
    return value.replace(/([^(:]+?:)/g, '<wbr>$1');
});

// ----------------------------------------------------------------------------
// Output to files

function outputHTML(path, template, obj, info) {
    info.root = urlPrefix;
    obj.root = urlPrefix;

    var yaml = buildYAML(info);
    var output = template.render(obj);

    mkdirp.sync(outputDir + path);
    fs.writeFileSync(outputDir + path + '/index.html', yaml + output);
}

function addLogo(svgData) {
    return svgData.replace(/(<g id="graph0"[^>]+translate\([0-9.]+ ([0-9.]+)\)">)/, 
                            '$1\n<g transform=\"translate(0 -$2)\">\n' + svgLogo + '\n</g>');

}

function customizeGraph(svgData) {
    return svgData.replace(/<title>[^<]+<\/title>/g, '')                    // get rid of titles
                  .replace(/ xlink:title="[^"]+"/g, ' target="_top"')       // and title attributes
                  .replace(/<polygon fill=\"white\"[^>]+>/g, '')            // clear background
                  .replace(/\/(type|protocol)\//g, urlPrefix + '/$1/')       // add root url 
}

function setMinimumWidth(svgData) {
    var match = /svg width=\"([^\"]+)pt\"/.exec(svgData),
        width = (match) ? match[1] : null;
        
    if (width && (width < 320)) {
        return svgData.replace(/(<svg width=\")([^\"]+)(pt\"[^>]+viewBox=\"\S+ \S+ )(\S+)/, '$01320$03320.0');
    } else {
        return svgData;
    }
}

function objectToYAML(obj) {
    var yaml = '---\n';
    for (key in obj) {
        yaml += key + ': "' + obj[key].replace('"', '&quot;') + '"\n';
    }
    yaml += '---\n\n';
    return yaml;
}

function getWrapperForGraph(type) {
    var info =  {
                    layout: 'svg_wrapper',
                    title: 'Inheritance Graph for ' + type.name,
                    typename: type.name,
                };

    return objectToYAML(info);
}

function outputGraph(path, dot, type) {
    var dotFile = outputDir + path + '/index.dot';
    var svgFile = outputDir + path + '/index.svg';

    // output dot-file
    mkdirp.sync(outputDir + path);
    fs.writeFileSync(dotFile, dot);
    
    // create and write the SVG
    var svgData = execsyncs('cat ' + dotFile + ' | /usr/local/bin/dot -Tsvg');
    svgData = addLogo(customizeGraph(svgData));
    fs.writeFileSync(svgFile, svgData);

    // create an HTML wrapper
    fs.writeFileSync(outputDir + path + '/index.html', getWrapperForGraph(type));
}

function generateItemInfo(item) {
    function skipEmpty(el) {
        return ((el != '') && (el != undefined));
    }

    function getName(item) {
        return item.name;
    }
    
    // title will just be the name
    var name = item.name;
    
    // meta description
    var description = '';
    if (item.comment) {
        var i = item.comment.search(/[\n\r]/);
        var comment = i == -1 ? item.comment : item.comment.substring(0, i);
        description = convertComment(comment);
    }
    if (description == '') {
        description = 'Swift documentation for ' + item.kind + ' \'' + name + '\'.';
    } else {
        description = description.replace(/<\/p>[^]*/, '')
                            .replace(/<[^>]+>/ig, '')
                            .replace(/\\ /g, '')
                            .replace(/\\\(/, '\\\\(')
                            .replace(/\.$/, '');
        description = 'Swift documentation for \'' + name + '\': ' + description + '.';
    }
    
    // meta keywords
    var keywords = [name, item.kind, 'swift', 'documentation'];
    if (item.functions)
        keywords.push.call(keywords, item.functions.map(getName));
    if (item.properties)
        keywords.push.call(keywords, item.properties.map(getName));
    if (item.aliases)
        keywords.push.call(keywords, item.aliases.map(getName));
    
    return { title: name, description: description, keywords: keywords.unique().filter(skipEmpty).join(',') };
}

function collectLinks(list, initial, linker, prefix) {
    if (!prefix) prefix = '';
    
    return list.reduce(function (p, c) {
        if (c.inherited == true) return p;
        if (p[prefix + c.name]) return p;
        p[prefix + c.name] = linker(c);
        return p;    
    }, initial);
}

function buildYAML(info) {
    // nothing to do here
    if (!info) return '';
    
    var yamlObject = { layout: (info.title == "Home") ? 'home' : 'default' };
    for (key in info) yamlObject[key] = info[key];

    // TODO: version number?
    
    return objectToYAML(yamlObject);
}

function build(context) {    
    // prepare local data
    var typesAsArray = Object.keys(context.types)
                             .sort()
                             .map(function(el) { return context.types[el] });
    var typeArray = typesAsArray.filter(function (el) { return el.kind != 'protocol' });
    var protocolArray = typesAsArray.filter(function (el) { return el.kind == 'protocol' });
    var functionNames = context.functions.map(function(el) { return el.name; }).unique().sort();
    var linkData = {};

    // this returns a function that adds a set prefix to whatever's passed in
    // handy for prepending URL paths onto type and function names below
    var prefixer = function(prefix) {
        return function(item) {
            return prefix + (item.uniqueSignatureURL || item.name);
        };
    };
    var functionPrefixer = function(prefix) {
        return function(item) {
            return prefix + item.name;
        };
    };

    // home page
    outputHTML('', templates.index_home, { 
        types: typeArray,
        protocols: protocolArray,
        operators: context.operators,
        functions: functionNames,
    }, pageInfo.home);
    
    // index pages
    outputHTML('protocol', templates.index_type, { items: protocolArray, path: 'protocol' }, pageInfo.index_protocol);
    outputHTML('type', templates.index_type, { items: typeArray, path: 'type' }, pageInfo.index_type);
    outputHTML('operator', templates.index_operator, { operators: context.operators }, pageInfo.index_operator);
    outputHTML('global', templates.index_global, { functions: functionNames }, pageInfo.index_global);
    outputHTML('func', templates.index_func, { functions: functionNames }, pageInfo.index_func);
    
    // types & protocols
    typesAsArray.forEach(function(el) {
        var info = generateItemInfo(el);
        var kind = (el.kind == 'protocol') ? el.kind : 'type';

        // get DOT for graphviz
        var dot = '';
        var hasHierarchy = false;
        if (outputGraphs) {
            dot = grapher.getTypeDOT(el.name, context);
            hasHierarchy = (dot.indexOf('->') != -1);
        }

        linkData = collectLinks(el.functions || [], linkData, prefixer(urlPrefix + '/' + kind + '/' + el.name + '/func#func-'), el.name + '.')
        linkData = collectLinks(el.properties || [], linkData, prefixer(urlPrefix + '/' + kind + '/' + el.name + '/prop#'), el.name + '.')
        linkData = collectLinks(el.cases || [], linkData, prefixer(urlPrefix + '/' + kind + '/' + el.name + '/case#'), el.name + '.')

        if (el.imp) {
            for (var impKey in el.imp) {
                var imp = el.imp[impKey];
                linkData = collectLinks(imp.functions || [], linkData, prefixer(urlPrefix + '/' + kind + '/' + el.name + '/func#func-' + impKey.urlify() + '-'), el.name + '.')
                linkData = collectLinks(imp.properties || [], linkData, prefixer(urlPrefix + '/' + kind + '/' + el.name + '/prop#' + impKey.urlify() + '-'), el.name + '.')
                linkData = collectLinks(imp.cases || [], linkData, prefixer(urlPrefix + '/' + kind + '/' + el.name + '/case#' + impKey.urlify() + '-'), el.name + '.')
            }
        }

        outputHTML('' + kind + '/' + el.name, templates.type, { type: el, hasHierarchy: hasHierarchy }, info);

        if (hasHierarchy)
            outputGraph('' + kind + '/' + el.name + '/hierarchy', dot, el);
    })
    
    // operators
    context.operators.forEach(function (el) {
        outputHTML('operator/' + el.slug, templates.operator, { operator: el }, {
            title: 'Operator: ' + el.name, 
            description: 'Swift documentation for the \'' + el.name + '\' operator.',
        });
    });
    
    // functions
    functionNames.forEach(function (funcName) {
        var functions = context.functions.filter(function(el) { return el.name == funcName });
        var info = generateItemInfo(functions[0]);
        outputHTML('func/' + funcName, templates.func, { functions: functions }, info);
    });

    // type aliases
    outputHTML('global/alias', templates.alias_list, { aliases: context.aliases }, pageInfo.alias_list);
    

    // operators need to have names converted for URL safety
    linkData = collectLinks(context.operators, linkData, function (item) { return urlPrefix + '/operator/' + item.slug; });

    // functions, types, & protocols are all simple
    linkData = collectLinks(context.functions, linkData, functionPrefixer(urlPrefix + '/func/'));
    linkData = collectLinks(typeArray, linkData, prefixer(urlPrefix + '/type/'));
    linkData = collectLinks(protocolArray, linkData, prefixer(urlPrefix + '/protocol/'));

    // variables and type aliases have only a single page each
    linkData = collectLinks(context.aliases, linkData, function() { return urlPrefix + '/global/alias/' });
    
    // build and write JS link data
    var linkDataString = 'var linkdata = ' + JSON.stringify(linkData) + ';\n';
    mkdirp.sync(outputDir + 'assets/js' );
    fs.writeFile(outputDir + 'assets/js/swift-linkdata.js', linkDataString);
}

// ----------------------------------------------------------------------------
// Export

var Builder = function() { };

Builder.prototype.build = build;

Builder.prototype.setOutputDir = function(dir) {
    outputDir = appRoot + dir.replace(/\/$/, '') + '/';
};

Builder.prototype.setURLPrefix = function(prefix) {
    urlPrefix = prefix.replace(/\/$/, '');
};

Builder.prototype.setOutputGraphs = function(output) {
    outputGraphs = output;
};

module.exports = new Builder();


