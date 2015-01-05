var Twig = require('twig'),
    mkdirp = require('mkdirp'),
    fs = require('fs'),
    spawn = require('child_process').spawn,
    grapher = require('./swiftdoc-grapher'),
    ext = require('./type-extensions'),
    markdown = require('markdown').markdown,
    PCRE = require('pcre').PCRE,
    
    twig = Twig.twig;

var DEBUG = false;
var svgLogo = fs.readFileSync('data/svg-logo.txt', 'utf8');
var outputDir = '';
var urlPrefix = '';
var dotPath = '/usr/local/bin/dot';

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
    var_list       : Twig.twig({ path: 'templates/var-list.html', async: false }),
    alias_list     : Twig.twig({ path: 'templates/alias-list.html', async: false }),

    operator       : Twig.twig({ path: 'templates/operator.html', async: false }),
    func           : Twig.twig({ path: 'templates/func.html', async: false }),
    type           : Twig.twig({ path: 'templates/type.html', async: false }),
                   
    index_func     : Twig.twig({ path: 'templates/index-func.html', async: false }),
    index_global   : Twig.twig({ path: 'templates/index-global.html', async: false }),
    index_home     : Twig.twig({ path: 'templates/index-home.html', async: false }),
    index_operator : Twig.twig({ path: 'templates/index-operator.html', async: false }),
    index_type     : Twig.twig({ path: 'templates/index-type.html', async: false }),
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
                .replace(/\.\. ([^:]+)::/g, '**$1:**')       // incorrect format for inline header?
                .replace(/:param: ([^ ,]+)/g, '**$1**')      // parameter
                .replace(/:returns: /g, '**Returns:** ')     // parameter
                .replace(/_,/g, ',')                         // Array has a weird internal link
                .replace(/\.\. _.+?:/g, '')
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

// convert operator names to words
function convertOperatorName(value) {
    var operator_map = {
        '.': 'dot',
        ':': 'cln',
        '?': 'qm',
        '/': 'slash',
        '|': 'bar',
        '<': 'lt',
        '>': 'gt',
        '=': 'eq',
        '-': 'mns',
        '+': 'pls',
        '*': 'star',
        '~': 'tilde',
        '&': 'amp',
        '%': 'pct',
        '^': 'crt',
        '!': 'excl',
        ' ': '_',
    };
    
    return value.split('')
                .map(function(el) {
                    return (operator_map[el]) ? operator_map[el] : el;
                })
                .filter(function(el) { return el != '' })
                .join('');
}

Twig.extendFilter('rst', convertComment);

Twig.extendFilter('opname', convertOperatorName);

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

// ----------------------------------------------------------------------------
// Output to files

function outputHTML(path, template, obj, info) {
    var yaml = buildYAML(info);
    var output = template.render(obj);
    mkdirp.sync(outputDir + path);
    fs.writeFile(outputDir + path + '/index.html', yaml + output);
}

function addLogo(svgData) {
    return svgData.replace(/(<g id="graph0"[^>]+translate\([0-9.]+ ([0-9.]+)\)">)/, 
                            '$1\n<g transform=\"translate(0 -$2)\">\n' + svgLogo + '\n</g>');

}

function customizeGraph(svgData) {
    return svgData.replace(/<title>[^<]+<\/title>/g, '')                   // get rid of titles
                  .replace(/ xlink:title="[^"]+"/g, ' target="_top"')      // and title attributes
                  .replace(/<polygon fill=\"white\"[^>]+>/g, '')           // clear background

                  // this is a no-op until versions are back
                  // .replace(/\/(type|protocol)\//, '/$1/')          // add root url 
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
    var process = spawn('/usr/local/bin/dot', ['-Tsvg'])
    var data = '';
    
    process.stdout.on('data', function (buffer) {
        data += buffer.toString();
    });

    process.on('close', function(code) {
        data = customizeGraph(data);
        data = addLogo(data);
        data = setMinimumWidth(data);
        
        // make the directory
        mkdirp.sync(outputDir + path);
        
        // write out the file
        fs.writeFile(outputDir + path + '/index.svg', data, function(err) {
            if (err) console.log(err);
        });
        
        // create an HTML wrapper
        fs.writeFile(outputDir + path + '/index.html', getWrapperForGraph(type), function(err) {
            if (err) console.log(err);
        });

    });

    process.stdin.write(dot);
    process.stdin.end();
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
    var description = convertComment(item.comment);
    if (!description) {
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

function collectLinks(list, initial, linker) {
    return list.reduce(function (p, c) {
        p[c.name] = linker(c.name);
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
        var dot = grapher.getTypeDOT(el.name, context);
        var hasHierarchy = (dot.indexOf('->') != -1);

        outputHTML('' + kind + '/' + el.name, templates.type, { type: el, hasHierarchy: hasHierarchy }, info);

        if (hasHierarchy)
            outputGraph('' + kind + '/' + el.name + '/hierarchy', dot, el);
    })
    
    // operators
    context.operators.forEach(function (el) {
        outputHTML('operator/' + convertOperatorName(el.name), templates.operator, { operator: el }, {
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
    
    // vars
    outputHTML('global/var', templates.var_list, { vars: context.properties }, pageInfo.var_list);

    // type aliases
    outputHTML('global/alias', templates.alias_list, { aliases: context.aliases }, pageInfo.alias_list);
    
    // this returns a function that adds a set prefix to whatever's passed in
    // handy for prepending URL paths onto type and function names below
    var prefixer = function(prefix) {
        return function(str) {
            return prefix + str;
        };
    };

    // operators need to have names converted for URL safety
    var linkData = collectLinks(context.operators, { }, function (name) { return '/operator/' + convertOperatorName(name); });

    // functions, types, & protocols are all simple
    linkData = collectLinks(context.functions, linkData, prefixer('/func/'));
    linkData = collectLinks(typeArray, linkData, prefixer('/type/'));
    linkData = collectLinks(protocolArray, linkData, prefixer('/protocol/'));

    // variables and type aliases have only a single page each
    linkData = collectLinks(context.properties, linkData, function() { return '/global/var/' });
    linkData = collectLinks(context.aliases, linkData, function() { return '/global/alias/' });
    
    var linkDataString = 'var linkdata = ' + JSON.stringify(linkData) + ';\n';
    mkdirp(outputDir + 'assets/js');
    fs.writeFile(outputDir + 'assets/js/swift-linkdata.js', linkDataString);
}

var Builder = function() { };

Builder.prototype.build = build;

Builder.prototype.setOutputDir = function(dir) {
    outputDir = dir.replace(/\/$/, '') + '/';
}

Builder.prototype.setURLPrefix = function(prefix) {
    urlPrefix = prefix;
}

module.exports = new Builder();


