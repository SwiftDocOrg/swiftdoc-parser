require('natural-compare-lite');
require('./type-extensions');

var extend = require('util')._extend;
var PCRE = require('pcre').PCRE;
    
var DEBUG = false;
var SHOW_UNMATCHED = true;
var currentComment = [];
var unmatched = [];

// ----------------------------------------------------------------------------
// Regular expressions

var regex = {
    
    // line with "}", closing type or protocol declaration
    close:      new PCRE("^\\s*\\}"),
    
    // single-line documentation comments: /// ...
    comment_single: new PCRE("^\\s*/// ?(.+)?"),
    
    // multi-line documentation comments: /** ... */
    comment_multi: new PCRE("^\\s*\\/\\*\\*\\s*\
                            (.+?)\
                            \\*\\/\\s*", PCRE.PCRE_EXTENDED | PCRE.PCRE_DOTALL),

    // operator declaration up to opening brace
    operator:   new PCRE("^(?'place'\\S+)\\s+operator\\s+(?'name'\\S+)\\s+{"),
    
    // associativity in operator declaration
    associativity: new PCRE("^\\s*associativity\\s+(\\S+)"),
    
    assignment: new PCRE("^\\s*assignment"),
    
    // precedence in operator declaration
    precedence: new PCRE("^\\s*precedence\\s+(\\S+)"),
    
    // operator function declaration 
    func_operator: new PCRE("^\\s*\
					(?'attribute'(@[^ ]+)\\s*)?\
					(?'place'(prefix|postfix)\\s*)?\
					(?'note'((private|public|internal)\\s*)*)\
					func\\s+\
					(?'name'([/|=+!*?%<>&|^~-]|\\.\\.)[^\\s(]*?)\
					(<(?'generic'[^<>]+?)>)?\\s*\
					(?'loop'\\((?'params'(?:[^()]*|(?&loop)){1,10})\\))\
					(\\s*(?'throws'(re)?throws)?)\
					(\\s*->\\s*(?'ret'.+))?\
					", PCRE.PCRE_EXTENDED),

    // function declaration (non-operator)
    func: new PCRE("^\\s*\
					(?'attribute'(@[^ ]+)\\s*)?\
					(?'note'((final|static|mutating|class|private|public|internal)\\s*)*)\
					func\\s+\
					(?'name'[^\\s<(]*)\
					(?'gloop'<(?'generic'(?:[^<>]*|(?&gloop)){1,10})>)?\\s*\
					(?'loop'\\((?'params'(?:[^()]*|(?&loop)){1,10})\\))\
					(\\s*(?'throws'(re)?throws)?)\
					(\\s*->\\s*(?'ret'.+))?\
					", PCRE.PCRE_EXTENDED),
    
    // single function parameter
	parameter: new PCRE("^\
                    ((?'note'.+)\\s+)??\
                    (?'name'[^\\s:]+)\\s*:\\s*\
                    ((?'type'[^=]+)(\\s*=\\s*(?'default'[^\\s]+))?)\
					", PCRE.PCRE_EXTENDED),
    
    param_split_1: new PCRE("(?'loop'\\((?:[^()]*|(?&loop)){1,3})$"),
    param_split_2: new PCRE("(?'loop'\\<(?:[^<>]*|(?&loop)){1,3})$"),
    
    // subscript declaration
	subscript: new PCRE("^\\s*\
					(?'accessibility'((private|public|internal)\\s*)*)?\
					subscript\\s*\
					(?'loop'\\((?'params'(?:[^()]*|(?&loop)){1,10})\\))\
					(\\s*->\\s*(?'ret'.+))?\
					", PCRE.PCRE_EXTENDED),
            
    // initializer declaration
	init: new PCRE("^\\s*\
					(?'note'(convenience|required|private|public|internal)\\s*)*\
					(?'init'init\\??)\\s*\
					(?'gloop'<(?'generic'(?:[^<>]*|(?&gloop)){1,10})>)?\\s*\
					(?'loop'\\((?'params'(?:[^()]*|(?&loop)){1,10})\\))\
					", PCRE.PCRE_EXTENDED),

    // deinitializer declaration
	deinit: new PCRE("^\\s*deinit", PCRE.PCRE_EXTENDED),

    // protocol declaration
	protocol: new PCRE("^\\s*\
					(?'attribute'((@[^ ]+)\\s*)*)?\
					(?'note'((private|public|internal)\\s*)*)?\
					protocol\\s*\
					(?'name'[a-zA-Z0-9_]+)\
					(\\s*:\\s*\
						(?'inherits'[a-zA-Z0-9_,\\s]+)\
					)?\\s*{\
					", PCRE.PCRE_EXTENDED),

    // struct / class / enum declaration
	type: new PCRE("^\\s*\
					(?'attribute'(@[^ ]+)\\s*)?\
					(?'note'(private|public|internal|final)\\s*)*\
					(?'kind'struct|class|enum)\\s+\
					(?'name'[^\\s<(]*)\
					(?'gloop'<(?'generic'(?:[^<>]*|(?&gloop)){1,10})>)?\
					(\\s*:\\s*\
						(?'inherits'[a-zA-Z0-9_,\\s]+)\
						(?'giloop'<(?'inheritgeneric'(?:[^<>]*|(?&giloop)){1,10})>)?\
					)?\\s*{\
					", PCRE.PCRE_EXTENDED),
					
    // extension to struct / class / enum
	extension: new PCRE("^\\s*\
					extension\\s+\
					(?'name'[^\\s<(]*)\
					(\\s*:\\s*\
						(?'inherits'[a-zA-Z0-9_,\\s]+)\
					)?\
                    (\\s*where\\s*\
					    (?'conforming'[^{]+)\
					)?\\s*{\
					", PCRE.PCRE_EXTENDED),
    
    // anything declared via var or let
	property: new PCRE("^\\s*\
					(?'final'(final)\\s*)*\
					(?'accessibility'((private|public|internal)\\s*)*)?\
					(?'class'class|static)?\\s*\
					(?'kind'var|let)\\s*\
					(?'name'[^:\\s]+)\\s*\
					:\\s*\
					(?'type'[^{\n]+)\\s*\
					(?'note'\\{([^\\}]+)\\})?\
					", PCRE.PCRE_EXTENDED),
    
    // a case inside an enum declaration
	enum_case: new PCRE("^\\s*\
					case\\s*\
					(?'name'[^(\\s]+)[ \t]*\
					(?'loop'\\((?'associated'(?:[^()]*|(?&loop)){1,10})\\))?\
					", PCRE.PCRE_EXTENDED),
	
	// any typealias				
	alias: new PCRE("^\\s*\
					(?'note'((private|public|internal)\\s*)*)?\
					typealias\\s+\
					(?'name'[^\\s=]+)\
					(\
						\\s*:\\s*\
						(?'proto'[^\n]+)\
					)?\
					(\
						\\s*=\\s*\
						(?'type'[^\n]+)\
					)?\
					", PCRE.PCRE_EXTENDED),
		
    // leftovers
    unmatched: new PCRE("^.+\n"),
}

// ----------------------------------------------------------------------------
// Useful functions

// capture the latest comment and clear
function useComment() {
    var c = currentComment.join("\n");
    currentComment = [];
    return c;
}

// extract everything that looks like a type name from a string, return as array
function typesFromString(str) {
    return str.split(/\b/)
            .filter(function(s) { return s.match(/^ *[_A-Z][a-z.0-9A-Z]+ *$/) })
            .map(function(s) { return s.trim() })
            .unique()
            .sort();
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

// from a dictionary of types, hoist all nested types up to the top level,
// prepending their names with that of their enclosing type.
//     struct String {
//         struct Index { }
//     }
//     should yield String and String.Index
function hoistNestedTypes(context) {
    // getting the keys here will prevent us from re-processing types 
    // that have already been hoisted
    var keys = Object.keys(context);
    
    for (var i = 0; i < keys.length; i++) {
        var type = context[keys[i]];
        
        // depth-first search: need to pull lowest-level types up first
        //     e.g, String.UnicodeScalarView.Index has to be hoisted to 
        //     UnicodeScalarView.Index inside String first
        if (typeof type.types == 'object') {
            hoistNestedTypes(type.types);
        } else {
            console.log(context[keys[i]]);
            continue;
        }
        
        for (var name in type.types) {            
            // update name and names of sub-types
            var subtype = type.types[name];
            subtype.name = type.name + '.' + subtype.name;
            if (subtype.types != null) {
                subtype.types = subtype.types.map(function(e) { return type.name + '.' + e; });
            } else {
                console.log(subtype.types);
            }
            
            // add to context
            context[subtype.name] = subtype;
        }
        type.types = Object.keys(type.types).map(function(e) { return type.name + '.' + e; });
    }
}

// split a string containing function parameters into an array of parameters,
// handling any cases where parameter types include commas
function splitParameters(paramString) {
	// can't get the regex for this right, so split on ", ", then check each
	var params = paramString.split(/\s*,\s*/);
	
	// loop down through parameter candidates
	for (var i = params.length - 1; i >= 0; i--) {
		// while we have an unmatched opening ( or < in this candidate
		while (regex.param_split_1.test(params[i]) || regex.param_split_2.test(params[i])) {
			// join with the following parameter
			params[i] = params[i] + ", " + params.splice(i + 1, 1);
		}
	}
	
	return params;
}

// ----------------------------------------------------------------------------
// Parsing / processing functions

function processGeneric(str) {
    // empty string gets empty property
    if (str == '') return {};

    return {
        line: str,
        types: typesFromString(str),
    };
}

function processParameters(str) {
    // empty string gets empty list
    if (str == '') return [];
    
    var params = splitParameters(str);
    var match;
    
    for (var i = 0; i < params.length; i++) {
        if (match = regex.parameter.exec(params[i])) {
            var obj = {
                name: params[i].ss(match.named.name),
                type: params[i].ss(match.named.type).trim(),
                note: params[i].ss(match.named.note),
                default: params[i].ss(match.named.default),
                types: typesFromString(params[i].ss(match.named.type)),
            }
            params[i] = obj;
        }
        else {
            console.log(params[i]);
        }
    }
    
    return params;
}

function processReturn(str) {
    // empty string gets empty property
    if (str == '') return {};

    return {
        line: str,
        types: typesFromString(str),
    };
}

function processInherits(str) {
    return str.split(/\s*,\s*/g)
        .map(function(e) { return e.trim(); })
        .filter(function(e) { return (e != ''); })
        .unique();
}

// recursively parse operator block 
function parseOperator(context, obj, data) {    
    data = data.trim();
    var match;

    // closing brace
    if (match = regex.close.exec(data)) {
        context.push(obj);
        return data.substr(match[1]);
    }
    
    if (match = regex.associativity.exec(data)) {
        obj.associativity = data.substring(match[2], match[3]);
        return parseOperator(context, obj, data.substr(match[1]));
    }
    
    if (match = regex.precedence.exec(data)) {
        obj.precedence = data.substring(match[2], match[3]);
        return parseOperator(context, obj, data.substr(match[1]));
    }
    
    if (match = regex.assignment.exec(data)) {
        obj.assignment = true
        return parseOperator(context, obj, data.substr(match[1]));
    }
    
    if (match = regex.unmatched.exec(data)) {
        return parseOperator(context, obj, data.substr(match[1]));
    }
}

// main parsing function - recursively consume all data
function parse(context, data) {
    if (DEBUG) console.log('beginning parse: data length: ' + data.length);
        
    while (data.length > 0) {
        data = data.trim();
        
        var match = null;
        
        // single-line comments
        if (match = regex.comment_single.exec(data)) {
            if (DEBUG) console.log('comment: ' + data.substring(match[0], match[1]));
    
            currentComment.push(data.substring(match[2], match[3]));
            data = data.substr(match[1]);
            continue;
        }
        
        // multi-line comments
        if (match = regex.comment_multi.exec(data)) {
            if (DEBUG) console.log('comment: ' + data.substring(match[0], match[1]));
    
            currentComment.push(data.substring(match[2], match[3]));
            data = data.substr(match[1]);
            continue;
        }
        
        // closing brace
        if (match = regex.close.exec(data)) {
            if (DEBUG) console.log('close: ' + data.substring(match[0], match[1]));
    
            return data.substr(match[1]);
            continue;
        }
    
        // operator
        if (match = regex.operator.exec(data)) {
            if (DEBUG) console.log('operator: ' + data.substring(match[0], match[1]));
            
            // use the place in the name for non-infix operators
            // +, -, but prefix ++
            var place = data.ss(match.named.place);
            var name = (place == 'infix') ? data.ss(match.named.name) : place + ' ' + data.ss(match.named.name);
    
            data = parseOperator(context.operators, { 
                kind:       'operator',
                place:      place,
                name:       name,
                slug:       convertOperatorName(name),
                assignment: false,
                associativity: '',
                precedence: '',
                comment:    useComment()
            }, data.substr(match[1]));
            continue;
        }
        
        // protocols
        if (match = regex.protocol.exec(data)) {
            if (DEBUG) console.log('protocol: ' + data.substring(match[0], match[1]));
    
            var name = data.ss(match.named.name);
            var protocol = { 
                kind:       'protocol',
                name:       name,
                slug:       name,
                inherits:   processInherits(data.ss(match.named.inherits)),
                inherited:  [],
                attr:       data.ss(match.named.attribute),
                operators:  [],
                functions:  [],
                types:      {},
                properties: [],
                aliases:    [],
                inits:      [],
                subscripts: [],
                imp:        {},
                comment:    useComment()
            }
    
            data = parse(protocol, data.substr(match[1]));
    
            context.types[name] = protocol;
            continue;
        }
        
        // types: struct / class / enum
        if (match = regex.type.exec(data)) {
            if (DEBUG) console.log('type: ' + data.substring(match[0], match[1]));
                    
            var name = data.ss(match.named.name);
            var type = { 
                kind:       data.ss(match.named.kind),
                name:       name,
                slug:       name,
                inherits:   processInherits(data.ss(match.named.inherits)),
                inherited:  [],
                generic:    processGeneric(data.ss(match.named.generic)),
                attr:       data.ss(match.named.attribute),
                note:       data.ss(match.named.note),
                operators:  [],
                functions:  [],
                types:      {},
                properties: [],
                aliases:    [],
                inits:      [],
                subscripts: [],
                comment:    useComment()
            }
    
            if (type.kind == 'enum') {
                type.cases = [];
            }
            
            data = parse(type, data.substr(match[1]));
    
            context.types[name] = type;
            continue;
        }
        
        // extension
        if (match = regex.extension.exec(data)) {
            if (DEBUG) console.log('extension: ' + data.substring(match[0], match[1]));
            
            var name = data.ss(match.named.name);
            var nestedTypeNames = name.split('.');

            var matchedType = nestedTypeNames.reduce(function (p, c) {
                if (!p) return p;
                return p.types[c];
            }, context);
            
            // didn't find a match - can happen in 3rd party frameworks
            // create the type, log an error message and continue, even though
            // this will get horked up when we hit the closing bracket of the unmatched extension
            if (!matchedType) {
                if (DEBUG) console.log('Note: extension for undeclared class "' + name + '".');
                matchedType = { 
                    kind:       'extension',
                    name:       name,
                    slug:       name,
                    inherits:   processInherits(data.ss(match.named.inherits)),
                    inherited:  [],
                    generic:    '',
                    attr:       '',
                    note:       '',
                    operators:  [],
                    functions:  [],
                    types:      {},
                    properties: [],
                    aliases:    [],
                    inits:      [],
                    subscripts: [],
                    comment:    useComment()
                }
    
                context.types[name] = matchedType;
            } else if (matchedType.kind == 'protocol') {
                
                // here we found a match, but it's a protocol -- these are default protocol implementations
                // introduced in Swift 2.0, and need to be grouped by any where conditions
                var conforming = (data.ss(match.named.conforming) == '') ? '*' : data.ss(match.named.conforming).trim();

                // remove leading Self -- Self.Generator.Element is the same as Generator.Element
                // however, Self by itself needs to stay
                conforming = conforming.replace(/Self\./g, '');

                // set up a new object for the type of conformance, if necessary
                if (! matchedType.imp[conforming]) {
                    matchedType.imp[conforming] = {
                        conforming: conforming,
                        name:       name,
                        operators:  [],
                        functions:  [],
                        types:      {},
                        properties: [],
                        aliases:    [],
                        inits:      [],
                        subscripts: [],
                    };
                }
                                
                // parse the enclosed definitions into the default implementations block
                // and then break to the next loop
                data = parse(matchedType.imp[conforming], data.substr(match[1]));
                continue;
                
            } else {
                // add protocols from this extension to the existing inherited types
                matchedType.inherits = matchedType.inherits.concat(processInherits(data.ss(match.named.inherits))).unique();
            }
    
            // continue parsing extended type
            data = parse(matchedType, data.substr(match[1]));
            continue;
        }
        
        // operator function
        if (match = regex.func_operator.exec(data)) {
            if (DEBUG) console.log('func_operator: ' + data.substring(match[0], match[1]));
    
            var name = data.ss(match.named.name);
    
            context.functions.push( { 
                kind:       'operator func',
                name:       name,
                slug:       convertOperatorName(name),
                generic:    processGeneric(data.ss(match.named.generic)),
                place:      data.ss(match.named.place).trim(),
                params:     processParameters(data.ss(match.named.params)),
                ret:        processReturn(data.ss(match.named.ret)),
                attr:       data.ss(match.named.attribute),
                line:       data.substring(match[0], match[1]).replace(/^@\S+\s/, ''),
                comment:    useComment()
            } );
                    
            data = data.substr(match[1]);
            continue;
        }
        
        // function
        if (match = regex.func.exec(data)) {
            if (DEBUG) console.log('function: ' + data.substring(match[0], match[1]));
            
            var name = data.ss(match.named.name);
            
            context.functions.push( { 
                kind:       'func',
                name:       name,
                slug:       name,
                generic:    processGeneric(data.ss(match.named.generic)),
                params:     processParameters(data.ss(match.named.params)),
                ret:        processReturn(data.ss(match.named.ret)),
                throws:     data.ss(match.named.throws).trim(),
                note:       data.ss(match.named.note).trim(),
                attr:       data.ss(match.named.attribute),
                line:       data.substring(match[0], match[1]).replace(/^@\S+\s/, ''),
                comment:    useComment()
            } );
                    
            data = data.substr(match[1]);
            continue;
        }
        
        // initializers
        if (match = regex.init.exec(data)) {
            if (DEBUG) console.log('init: ' + data.substring(match[0], match[1]));
    
            context.inits.push( { 
                kind:       'init',
                generic:    processGeneric(data.ss(match.named.generic)),
                params:     processParameters(data.ss(match.named.params)),
                init:       data.ss(match.named.init),
                note:       data.ss(match.named.note),
                comment:    useComment()
            } );
                    
            data = data.substr(match[1]);
            continue;
        }
        
        // deinitializers
        if (match = regex.deinit.exec(data)) {
            if (DEBUG) console.log('deinit: ' + data.substring(match[0], match[1]));
    
            context.inits.push( { 
                kind:       'deinit',
                comment:    useComment()
            } );
                    
            data = data.substr(match[1]);
            continue;
        }
        
        // subscripts
        if (match = regex.subscript.exec(data)) {
            if (DEBUG) console.log('subscript: ' + data.substring(match[0], match[1]));
    
            context.subscripts.push( { 
                kind:       'subscript',
                params:     processParameters(data.ss(match.named.params)),
                ret:        processReturn(data.ss(match.named.ret)),
                comment:    useComment()
            } );
                    
            data = data.substr(match[1]);
            continue;
        }
        
        // aliases
        if (match = regex.alias.exec(data)) {
            if (DEBUG) console.log('aliases: ' + data.substring(match[0], match[1]));
    
            context.aliases.push( { 
                kind:       'typealias',
                name:       data.ss(match.named.name),
                type:       data.ss(match.named.type),
                proto:      data.ss(match.named.proto),
                comment:    useComment()
            } );
            
            data = data.substr(match[1]);
            continue;
        }
        
        // cases
        if (match = regex.enum_case.exec(data)) {
            if (DEBUG) console.log('cases: ' + data.substring(match[0], match[1]));
    
            context.cases.push( { 
                kind:       'case',
                name:       data.ss(match.named.name),
                associated: data.ss(match.named.associated),
                subtypes:   typesFromString(data.ss(match.named.associated)),
                comment:    useComment()
            } );
                    
            data = data.substr(match[1]);
            continue;
        }
        
        // variable / property
        if (match = regex.property.exec(data)) {
            if (DEBUG) console.log('property: ' + data.substring(match[0], match[1]));
    
            var note = data.ss(match.named.note);
            var kind = data.ss(match.named.kind);
            
            // readonly if note is non-empty and doesn't contain set, or if a "let" var
            //    e.g. var foo: Int { get }        <- readonly
            //         var bar: Int { get set }    <- nope
            //         var baz: Int                <- nope
            //         let zaz: Int                <- readonly again
            var readonly = (((note != '') && (note.indexOf('set') == -1)) || (kind == 'let'));
            
            context.properties.push( {
                kind:       'var',
                note:       data.ss(match.named.note),
                final:      data.ss(match.named.final),
                name:       data.ss(match.named.name),
                type:       data.ss(match.named.type).trim(),
                readonly:   readonly,
                stat:       data.ss(match.named.class),
                subtypes:   typesFromString(data.ss(match.named.type)),
                comment:    useComment()
            } );
            
            data = data.substr(match[1]);
            continue;
        }
        
        // keep track of unmatched lines
        if (match = regex.unmatched.exec(data)) {
            if (DEBUG || SHOW_UNMATCHED) console.log('unmatched: ' + data.substring(match[0], match[1]));
            
            unmatched.push(data.substring(match[0], match[1]));
            data = data.substr(match[1]);
            continue;
        }
    
    } // while data.length > 0
    
    if (DEBUG) console.log('exiting parse');
}

// ----------------------------------------------------------------------------
// Export

var Parser = function() { };

Parser.prototype.parse = function(data, context) {
    // pre-parsing data cleanup
    data = data.replace(/public\s*/g, '')
                .replace(/\@warn_unused_result(\([^)]+\))?\s*/g, '')
                .replace(/\@rethrows\s*/g, '');
    
    // initialize context for parsing
    var parsedData = context || {
        operators: [],
        functions: [],
        properties: [],
        aliases: [],
        types: {},
    }
    
    // parse all the things
    parse(parsedData, data);
    
    // clear any unused comments 
    currentComment = [];
        
    return parsedData;
}

Parser.prototype.finalize = function(context) {
    // bring nested types up to the top level, prepending parent type's name
    hoistNestedTypes(context.types);

    // collect inherited types
    Object.keys(context.types).forEach(function(t) {
        context.types[t].inherits.forEach(function(i) {
            if (context.types[i] == undefined) {
                if (DEBUG) console.log('Note: In "' + t + '", can\'t find "' + i + '".');
                return;
            }
            context.types[i].inherited.push(t); 
        });
    });
    
    // copy top-level operator functions underneath operator declarations
    context.operators.forEach(function(op) {
        op.functions = context.functions.filter(function(f) {
            var name = (f.place != '') ? f.place + ' ' + f.name : f.name;
            return (name == op.name);
        }).sort(function(a, b) {
            return String.naturalCompare(a.line, b.line);
        });
    });
    
    // strip operator functions from top-level functions
    context.functions = context.functions.filter(function(f) {
        return (f.kind == 'func');
    }).sort(function(a, b) {
        if (a.line.length != b.line.length) {
            return a.line.length - b.line.length;
        }
        return String.naturalCompare(a.line, b.line);
    });
    
    // sort operators and functions by place and name
    context.operators.sort(function(a, b) {
        var placeScore = { infix: 0, prefix: 1, postfix: 2 };
        var aScore = placeScore[a.place];
        var bScore = placeScore[b.place];
        
        if (aScore == bScore) {
            return a.name.localeCompare(b.name);
        } else {
            return aScore - bScore;
        }
    })
    
    return context;
}

module.exports = new Parser();

