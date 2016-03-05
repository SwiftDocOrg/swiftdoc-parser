require('./type-extensions');

var DEBUG = false;

// name to show in the dot graph
// include generic type: Array<T>
function dotNameForType(type, currentTypeName) {
    if (currentTypeName && (currentTypeName != type.name)) {
        return type.name;
    }
    return type.name + (!((type.generic) && (type.generic.line)) ? '' : '<' + type.generic.line + '>');
}

function dotPrefix(title) {
    return 'strict digraph "' + title + ' - SwiftDoc.org" {\n\
    pad="0.1,0.8"\n\
    node [shape=box, style="filled,rounded", color="#999999", fillcolor="#999999", fontcolor=white, fontname=Helvetica, fontnames="Helvetica,sansserif", fontsize=12, margin="0.07,0.05", height="0.3"]\n\
    edge [color="#cccccc"]\n';
}

// beginning of DOT
function dotPrefixForType(type) {
    return dotPrefix(dotNameForType(type, null) + ' - Type Hierarchy');
}

// beginning of DOT
function dotPrefixForStdLib() {
    return dotPrefix('Standard Library Type Hierarchy');
}

// end of DOT - includes subgraph for all the (non-protocol) types so they end up on one line
function dotPostfixWithTypes(typeNames) {
    var str = typeNames.map(function(el) { return '"' + el + '";' }).join(' ');
    return '\n\
    subgraph Types {\n\
        rank = max; ' + str + '\n\
    }\n}';
}

// return array of "tuples" (2-element arrays in this case) with all inheritance 
// relationships above type given in `typeName`
function collectInherits(typeName, types) {
    if (!(types[typeName])) return [];

    var tuples = types[typeName].inherits.map(function(el) { return [el, typeName] });
    types[typeName].inherits.forEach(function(el) {
        tuples.extend(collectInherits(el, types));
    });
    
    return tuples;
}

// return array of "tuples" (two-member arrays in this case) with all inheritance 
// relationships below type given in `typeName`
function collectInherited(typeName, types) {
    if (!(types[typeName])) return [];

    var tuples = types[typeName].inherited.map(function(el) { return [typeName, el] });
    types[typeName].inherited.forEach(function(el) {
        tuples.extend(collectInherited(el, types));
    });
    
    return tuples;
}

function buildTypeDOT(typeName, types) {
    // build an array of tuples (2-element arrays, here) with 
    // start and endpoints for paths of inheritance
    var paths = collectInherited(typeName, types)
                    .extend(collectInherits(typeName, types));

    // map tuples onto DOT edge syntax 
    // e.g., "_CollectionType" -> "CollectionType"
    var dotPaths = paths.map(function(el) {
        var nameFrom = (types[el[0]]) ? dotNameForType(types[el[0]], typeName) : el[0];
        var nameTo   = (types[el[1]]) ? dotNameForType(types[el[1]], typeName) : el[1];
        return '    "' + nameFrom + '" -> "' + nameTo + '"';
    });
    
    // build list of names of types (struct, class, enum) only
    var typeNames = paths.reduce(function(p, c) { return p.extend(c); }, [])
                         .unique()
                         .filter(function(el) { return (types[el] != null); })
                         .filter(function(el) { return (types[el].kind != 'protocol'); })
                         .map(function (el) { return dotNameForType(types[el], typeName);
                         });
    
    // map flattened, uniqued tuples to DOT node syntax
    // e.g. "CollectionType" [URL="/protocol/CollectionType/"]
    var dotNodes = paths.reduce(function(p, c) { return p.extend(c); }, [])
                        .unique()
                        .sort()
                        .filter(function(el) { return (types[el] != null); })
                        .map(function(el) {
                            var kind = (types[el].kind == 'protocol') ? 'protocol' : 'type';
                            
                            if (el == typeName) {
                                return '    "' + dotNameForType(types[el], typeName) + 
                                        '" [URL="/' + kind + '/' + el + '/",' +
                                        ' style="filled' + 
                                        ((kind == 'protocol') ? ',rounded' : '') + 
                                        '", fillcolor="#ee543d", color="#ee543d"]';
                            }
                            
                            if (kind == 'protocol') {
                                return '    "' + types[el].name + 
                                        '" [URL="/protocol/' + el + '/hierarchy/"]';
                            }
                            
                            return '    "' + types[el].name + 
                                    '" [URL="/type/' + el + '/hierarchy/", style=filled]';
                        });
    
    return dotPrefixForType(types[typeName]) + 
           dotNodes.join('\n') + 
           dotPaths.join('\n') + 
           dotPostfixWithTypes(typeNames);
}

// ----------------------------------------------------------------------------
// Export

var Grapher = function() { };

Grapher.prototype.getTypeDOT = function(typeName, context) {
    return buildTypeDOT(typeName, context.types);
}

Grapher.prototype.getAllTypesDOT = function(context) {
    var rslt = {};
    for (var typeName in context.types) {
        rslt[typeName] = buildTypeDOT(typeName, context.types);
    }
    return rslt;
}

module.exports = new Grapher();

