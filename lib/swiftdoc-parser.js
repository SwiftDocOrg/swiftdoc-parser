var ncl = require('natural-compare-lite');
require('./type-extensions');

if (ncl !== undefined) {
	String.naturalCompare = ncl;
}

var extend = require('util')._extend;
var PCRE = require('pcre').PCRE;
var fs = require('fs');
    
var DEBUG = false;
var SHOW_UNMATCHED = true;
var DEBUG_EXTENSIONS = true;
var currentComment = [];
var unmatched = [];

var API = {
    iterator: 'generator',
    Iterator: 'Generator', 
    makeIterator: 'generate', 
    Sequence: 'SequenceType', 
    Collection: 'CollectionType',
    };

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

    // precedence group declaration
    precedencegroup: new PCRE("^\\s*precedencegroup\\s+(?'name'\\S+)\\s+{"),
    
    // operator declaration up to opening brace
    operator:   new PCRE("^(?'place'\\S+)\\s+operator\\s+(?'name'\\S+)\\s+{"),
    
    // Swift 3 operator declaration
    // e.g., `infix operator - : AdditionPrecedence` or `prefix operator !`
    swift3_operator: new PCRE("^\\s*\
                              (?'fixity'infix|prefix|postfix)\
                              \\s+operator\\s+\
                              (?'operator'\\S+)\
                              (\\s*:\\s*(?'group'\\S+))?", PCRE.PCRE_EXTENDED | PCRE.PCRE_DOTALL),
    
    // associativity in operator declaration
    associativity: new PCRE("^\\s*associativity:\\s+(\\S+)"),
    
    assignment: new PCRE("^\\s*assignment:\\s+(\\S+)"),
    
    // precedence in operator declaration
    higher_than: new PCRE("^\\s*higherThan:\\s+(\\S+)"),
    
    // operator function declaration 
    func_operator: new PCRE("^\\s*\
					(?'attribute'(@[^ ]+)\\s*)?\
					(?'place'(prefix|postfix)\\s*)?\
					(?'note'((static|private|public|internal)\\s*)*)\
					func\\s+\
					(?'name'([/|=+!*?%<>&|^~-]|\\.\\.)[^\\s(]*?)\\s*\
					(<(?'generic'[^<>]+?)>)?\\s*\
					(?'loop'\\((?'params'(?:[^()]*|(?&loop)){1,10})\\))\
					(\\s*(?'throws'(re)?throws)?)\
					(\\s*->\\s*(?'ret'.+))?\
					(\\s*where\\s+(?'where'.+))?\
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
					(\\s*where\\s+(?'where'.+))?\
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
					(?'gloop'<(?'generic'(?:[^<>]*|(?&gloop)){1,10})>)?\\s*\
					(?'loop'\\((?'params'(?:[^()]*|(?&loop)){1,10})\\))\
					(\\s*->\\s*(?'ret'.+))?\
					(\\s*where\\s+(?'where'.+))?\
					", PCRE.PCRE_EXTENDED),
            
    // initializer declaration
	init: new PCRE("^\\s*\
					(?'note'(convenience|required|private|public|internal)\\s*)*\
					(?'init'init\\??)\\s*\
					(?'gloop'<(?'generic'(?:[^<>]*|(?&gloop)){1,10})>)?\\s*\
					(?'loop'\\((?'params'(?:[^()]*|(?&loop)){1,10})\\))\
					(\\s*(?'throws'(re)?throws)?)\
					(\\s*where\\s+(?'where'.+))?\
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
						(?'inherits'[a-zA-Z0-9_,:.=\\s]+?)\
						(where\\s+(?'where'[a-zA-Z0-9_,.:=\\s]+)?)?\
					)?\\s*{\
					", PCRE.PCRE_EXTENDED),

    // struct / class / enum declaration
	type: new PCRE("^\\s*\
					(?'attribute'(@[^ ]+)\\s*)?\
					(?'note'(open|private|public|internal|final)\\s*)*\
					(?'kind'struct|class|enum)\\s+\
					(?'name'[^\\s<(]*)\
					(?'gloop'<(?'generic'(?:[^<>]*|(?&gloop)){1,10})>)?\
					(\\s*:\\s*\
						(?'inherits'[a-zA-Z0-9_,\\s]+)\
						(?'giloop'<(?'inheritgeneric'(?:[^<>]*|(?&giloop)){1,10})>)?\
					)?\
					(\\s*where\\s*(?'where'.+))?\
					\\s*{\
					", PCRE.PCRE_EXTENDED),
					
    // extension to struct / class / enum
	extension: new PCRE("^\\s*\
					extension\\s+\
					(?'name'[^\\s<(]*)\
					(\\s*:\\s*\
						(?'inherits'[a-zA-Z0-9_,.\\s]+)\
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
	
	// any typealias or associated type
	alias: new PCRE("^\\s*\
					(?'note'((private|public|internal)\\s*)*)?\
					(typealias|associatedtype)\\s+\
					(?'name'[^\\s=]+)\
					(\
						\\s*:?\\s*\
						(?'proto'[][a-zA-Z0-9.,_\\t <>=():?&]+)\
					)?\
					", PCRE.PCRE_EXTENDED),
	
	alias_where: new PCRE("^(.+)\\s+where\\s+(.+)", PCRE.PCRE_EXTENDED),
	alias_default: new PCRE("^(.*)\\s*=\\s*(.+)", PCRE.PCRE_EXTENDED),
	
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
            // console.log(context[keys[i]]);
            continue;
        }
        
        for (var name in type.types) {            
            // update name and names of sub-types
            var subtype = type.types[name];
            subtype.name = type.name + '.' + subtype.name;
            if (subtype.types != null) {
                subtype.types = subtype.types.map(function(e) { return type.name + '.' + e; });
            } else {
                // console.log(subtype.types);
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
    if (str == '') return { formatted: '' };
    
    var trimmed = str.replace(/ *where.+/g, '');
    trimmed = trimmed.replace(/\s*:\s*[^,]+/g, '');
    
    return {
        line: str,
        formatted: '<' + trimmed + '>',
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
            console.log('Unmatched parameters: ');
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

// recursively parse operator block 
function parsePrecedenceGroup(context, obj, data) {    
    data = data.trim();
    var match;

    // closing brace
    if (match = regex.close.exec(data)) {
        context[obj.name] = obj;
        return data.substr(match[1]);
    }
    
    if (match = regex.associativity.exec(data)) {
        obj.associativity = data.substring(match[2], match[3]);
        return parsePrecedenceGroup(context, obj, data.substr(match[1]));
    }
    
    if (match = regex.higher_than.exec(data)) {
        obj.higherThan = data.substring(match[2], match[3]);
        return parsePrecedenceGroup(context, obj, data.substr(match[1]));
    }
    
    if (match = regex.assignment.exec(data)) {
        obj.assignment = data.substring(match[2], match[3]) == 'true';
        return parsePrecedenceGroup(context, obj, data.substr(match[1]));
    }
    
    if (match = regex.unmatched.exec(data)) {
        return parsePrecedenceGroup(context, obj, data.substr(match[1]));
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
            // falling back to built-in RegExp matching, since the PRCE-based
            // regex fails at calculating Unicode substring lengths
            var commentMatch = data.match(/\/\/\/ ?(.*)\n/);
            if (DEBUG) console.log('comment: ' + commentMatch[1]);
    
            currentComment.push(commentMatch[1]);
            data = data.substr(commentMatch[0].length + commentMatch.index);
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
    
        // precedence group
        if (match = regex.precedencegroup.exec(data)) {
            if (DEBUG) console.log('precedence_group: ' + data.substring(match[0], match[1]));
            
            var name = data.ss(match.named.name);
            data = parsePrecedenceGroup(context.precedencegroups, { 
                kind:       'precedence_group',
                name:       name,
                slug:       name,
                assignment: false,
                associativity: '',
                higherThan: '',
                comment:    useComment()
            }, data.substr(match[1]));
            continue;
        }

        // Swift 3 operator
        if (match = regex.swift3_operator.exec(data)) {
            if (DEBUG) console.log('Swift 3 operator: ' + data.substring(match[0], match[1]));

            // use the place in the name for non-infix operators
            // +, -, but prefix ++
            var place = data.ss(match.named.fixity);
            var name = (place == 'infix') ? data.ss(match.named.operator) : place + ' ' + data.ss(match.named.operator);
            
            var group = match.named.group ? data.ss(match.named.group) : '';
            if (context.precedencegroups[group]) {
                context.operators.push({
                    kind:       'operator',
                    name:       name,
                    slug:       convertOperatorName(name),
                    assignment: context.precedencegroups[group].assignment,
                    associativity: context.precedencegroups[group].associativity,
                    group:      group,
                    operators: [],
                    comment:    useComment()
                });
            } else if (group == '') {
                context.operators.push({
                    kind:       'operator',
                    name:       name,
                    slug:       convertOperatorName(name),
                    assignment: '',
                    associativity: '',
                    group: '',
                    operators: [],
                    comment:    useComment()
                });
            } else {
                console.log('Unmatched precedence group ' + group + ' in "' + name + '"');
            }
            
            data = data.substr(match[1]);
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
            // console.log('protocol: ' + data.substring(match[0], match[1]));
             
            var name = data.ss(match.named.name);
            var protocol = { 
                kind:       'protocol',
                name:       name,
                slug:       name,
                inherits:   processInherits(data.ss(match.named.inherits)),
                inherited:  [],
                allInherited:[],
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
                allInherited:[],
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
            
            // clear current comment
            currentComment = [];

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
                    allInherited:[],
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
                        urlprefix: conforming.urlify() + '-',
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
                line:       data.substring(match[0], match[1]).replace(/^@\S+\s/, ''),
                comment:    useComment()
            } );
                    
            data = data.substr(match[1]);
            continue;
        }
        
        // aliases
        if (match = regex.alias.exec(data)) {
            if (DEBUG) console.log('aliases: ' + data.substring(match[0], match[1]));
            
            var proto = data.ss(match.named.proto).trim();
            var where = '';
            var type = '';
            if (linematch = regex.alias_where.exec(proto)) {
                where = proto.substring(linematch[4], linematch[5]);
                proto = proto.substring(linematch[2], linematch[3]);
            }
            if (linematch = regex.alias_default.exec(proto)) {
                type = proto.substring(linematch[4], linematch[5]);
                proto = proto.substring(linematch[2], linematch[3]);
            }
            
            context.aliases.push( { 
                kind:       'typealias',
                name:       data.ss(match.named.name),
                type:       type,
                proto:      proto,
                where:      where,
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

function findMinimumImplementation(protocol, context) {
    var protocolContext = (JSON.parse(JSON.stringify(context.types[protocol])));
    protocolContext.functions.push("Hello!")

    var protocolContext = { 
        operators:  [],
        functions:  [],
        properties: [],
        aliases:    [],
        inits:      [],
        subscripts: [],
    }
}

// ----------------------------------------------------------------------------
// Finalizing processing helpers

function deepCopy(x) {
    // so embarrassed for javascript right now
    return JSON.parse(JSON.stringify(x));
}

function withSpaceBefore(item) {
    if (item && (item != '')) {
        return ' ' + item;
    } else {
        return '';
    }
}

function withSpaceAfter(item) {
    if (item && (item != '')) {
        return item + ' ';
    } else {
        return '';
    }
}

function removeGenericClause(str) {
    var idx = str.indexOf('<');
    if (idx != -1) {
        return str.substring(0, idx);
    }
    return str;
}

function reduceParameters(includingTypes, isInit) {
    return function(c, p, index) {
        // if we're including types and the string in progress isn't blank, add a comma
        var result = (includingTypes && (c != '')) ? ', ' : '';
        
        // add the external parameter name and a colon
        result += externalParameterName(p, ((index == 0) && !isInit)) + ':';
        
        // add the type if required
        if (includingTypes) result += ' ' + p.type;
        
        return c + result;
    }
}

function externalParameterName(param, first) {
    var ext = param.note ? param.note : '';
    ext = ext.replace(/@\w+/, '');
    if (ext == '') {
        return first ? '_' : param.name;
    } else {
        return ext;
    }
}

function signatureForFunction(f, includingTypes, slugsForNames) {
    // mush together the external parameter names for each parameter
    var params = f.params.reduce(reduceParameters(includingTypes), '');
    var name = slugsForNames ? f.slug : f.name;
    
    if (includingTypes && f.generic && f.generic.line) {
        return name + 
                (f.kind == 'operator func' ? ' <' : '<') +
                f.generic.line + '>(' + params + ')';
    }
    return name + '(' + params + ')';
}

function declarationForFunction(f) {
    // mush together all the parameters
    var params = f.params.reduce(function(c, p, index) {
        // if the string in progress isn't blank, add a comma
        var result = (c != '' ? ', ' : '') +
        
        // note
        (p.note == '' ? '' : p.note + ' ') +
        
        // name & type
        p.name + ': ' + p.type +
        
        // default
        (p.default == '' ? '' : ' = ' + p.default);
        
        return c + result;
    }, '');
    
    var generic = (f.generic && f.generic.line) ? '<' + f.generic.line + '>' : '';
    var ret = (f.ret && f.ret.line) ? ' -> ' + f.ret.line : '';
    
    return withSpaceAfter(f.attr) + withSpaceAfter(f.note) + withSpaceAfter(f.place) +
            'func ' + f.name + generic + '(' + params + ')' + withSpaceBefore(f.throws) + ret;
}

function signatureForInit(f, includingTypes) {
    // bail if we don't have params
    if (!f.params) return f.kind;
    
    // mush together the external parameter names for each parameter
    var params = f.params.reduce(reduceParameters(includingTypes, true), '');

    if (includingTypes && f.generic && f.generic.line) {
        return f.init + '<' + f.generic.line + '>(' + params + ')';
    }
    return f.init + '(' + params + ')'    
}

function declarationForInit(f) {
    // bail if we don't have params
    if (!f.params) return f.kind;

    // mush together all the parameters
    var params = f.params.reduce(function(c, p, index) {
        // if the string in progress isn't blank, add a comma
        var result = (c != '' ? ', ' : '') +
        
        // note
        (p.note == '' ? '' : p.note + ' ') +
        
        // name & type
        p.name + ': ' + p.type +
        
        // default
        (p.default == '' ? '' : ' = ' + p.default);
        
        return c + result;
    }, '');

    var generic = (f.generic && f.generic.line) ? '<' + f.generic.line + '>' : '';
    return f.init + generic + '(' + params + ')'    
}

function finalizeItems(items, signatureFunction, declarationFunction) {
    // generate signatures and build list of counts per signature
    var signatures = items.map(function(item) { return signatureFunction(item, false); });
    var signatureCounts = signatures.reduce(function(c, p) {
        if (c[p]) {
            c[p] = c[p] + 1;
        } else {
            c[p] = 1;
        }
        return c;
    }, {});
        
    // for each item, check to see if the signature is unique
    // if not, we'll require the type to be shown as part of the signature
    items.forEach(function(item, index) {
        item.signature = signatures[index];
        if (signatureCounts[item.signature] > 1) {
            item.requireTypes = true;
        }
    });
    
    // add the types to item signatures where required
    items.forEach(function(item, index) {
        item.uniqueSignature = signatureFunction(item, item.requireTypes);
        item.declaration = declarationFunction(item);
        item.uniqueSignatureURL = signatureFunction(item, item.requireTypes, true).urlify();
        item.declarationURL = item.declaration.urlify();
    });   
    
    // sort the items based on signature *without* types
    items.sort(sortBySignature);

    return items;
}

function finalizeProperties(properties) {

    properties.forEach(function(p) {
        p.signature = withSpaceAfter(p.stat) + withSpaceAfter(p.kind) + p.name;
        p.uniqueSignature = p.signature + ': ' + p.type;
        p.uniqueSignatureURL = p.uniqueSignature.urlify();

        var setter = p.readonly ? '' : 'set';
        p.declaration = p.signature + ': ' + p.type + ' { get ' + withSpaceAfter(setter) + '}';
        p.declarationURL = p.declaration.urlify();
    });
    
    properties.sort(sortByNameOrLength);
    
    return properties;
}

function finalizeSubscripts(subscripts) {

    subscripts.forEach(function(s) {
        s.line = s.line.replace(/subscript */, 'subscript');
        
        s.signature = s.line.replace(/\([^:]+/, '(_').replace(/\s*->.+/, '');
        s.uniqueSignature = s.signature; // s.line.replace(/\([^:]+/, '(_');
        s.uniqueSignatureURL = s.uniqueSignature.urlify();

        s.declaration = s.line;
        s.declarationURL = s.declaration.urlify();
    });
    
    subscripts.sort(sortByNameOrLength);
    
    return subscripts;
}

function findWithMatchingSignature(toFind, itemList) {
    for (i = 0; i < itemList.length; i++) {
        if ((itemList[i].signature == toFind.signature) || 
            (itemList[i].signature == toFind.signature.replace('init?', 'init'))) {
            return i;
        }
    }
    return -1;
}

function findWithMatchingUniqueSignature(toFind, itemList) {
    var toFindKey = toFind.uniqueSignature || toFind.name;
    for (i = 0; i < itemList.length; i++) {
        if (((itemList[i].uniqueSignature || itemList[i].name) == toFindKey) ||
            (itemList[i].uniqueSignature == toFindKey.replace('init?', 'init'))) {
            return i;
        }
    }
    return -1;
}

function containsWithMatchingSignature(toFind, itemList) {
    return findWithMatchingSignature(toFind, itemList) != -1;
}

function sortByNameOrLength(a, b) {
    var comp = String.naturalCompare(a.name, b.name);
    if (comp != 0) { return comp; }
    if (a.line && b.line) {
        return a.line.length - b.line.length;
    }
    if (a.uniqueSignature && b.uniqueSignature) {
        return a.uniqueSignature.length - b.uniqueSignature.length;
    }
    return 0;
}

function sortBySignature(a, b) {
    // first, compare basic signatures for grouped sorting
    var result = String.naturalCompare(a.signature, b.signature);
    if (result != 0) return result;
    
    // next, sort generics after non-generics
    var aGen = ((a.generic && a.generic.line) != undefined), bGen = ((b.generic && b.generic.line) != undefined);
    if (aGen != bGen) return bGen ? -1 : 1;
    
    // within generics, sort by length of the generic line
    if (aGen && bGen) {
        result = a.generic.line.length - b.generic.line.length;
        if (result != 0) return result;
    }
    
    // finally, order by unique signature (adds generics & types)
    return String.naturalCompare(a.uniqueSignature, b.uniqueSignature);
}

function checkConformance(context, type, typeToCheck) {
    // make a copy of original list of inherits
    var checklist = context.types[type].inherits.slice();
//1console.log('Looking for ' + typeToCheck + ' in ' + type);

    var i = 0;
    while (i < checklist.length) {
//1console.log('Current list: ' + checklist);
        // did we match?
        if (checklist[i] == typeToCheck) {
            return true;
        }
        
        // make sure the type exists
        if (!(context.types[checklist[i]])) {
            i++;    
            continue;
        }
        
        // add unique inherited symbols to the checklist
        context.types[checklist[i]].inherits.forEach(function(inherit) {
            if (checklist.indexOf(inherit) == -1) {
                checklist.push(inherit);
            }
        });
        
        i++;
    }
    
    return false;
}

function findInheritedAlias(context, type, alias) {
    // make a copy of original list of inherits
    var checklist = context.types[type].inherits.slice();

    var i = 0;
    while (i < checklist.length) {
        // make sure the type exists
        if (!(context.types[checklist[i]])) {
            i++;
            continue;
        }
        
        // find any matching aliases
        var matchingAlias = context.types[checklist[i]].aliases.filter(function(element) {
            return (element.name == alias);
        });
        
        // did we find a match?
        if (matchingAlias.length > 0) {
            return matchingAlias[0];
        }
        
        // add unique inherited symbols to the checklist
        context.types[checklist[i]].inherits.forEach(function(inherit) {
            if (checklist.indexOf(inherit) == -1) {
                checklist.push(inherit);
            }
        });

        i++;
    }
    
    return {};
}

// when the where clause passes, we can bring these implementations right onto the type
// e.g., every Array has Index : RandomAccessIndexType, so that where clause passes
var kWhereClausePass = 0;
// when the where clause fails, we don't include the implementations even in the default
// implementations section
// e.g., for Array, SubSequence == ArraySlice<Element>, so SubSequence == Self would fail
var kWhereClauseFail = 1;
// this is for other cases, where we don't have enough information to solve or it depends on
// the generic specialization of the instance
// e.g., only some Arrays have Generator.Element : Equatable, so that should just live in the
// default implementations section
var kWhereClauseUnknown = 2;

function satisfiesWhereClause(context, type, whereClause) {
    // Whitelist for unconstrained extensions
    if (whereClause == '*') return kWhereClausePass;
    
    // Whitelist this -- it's basically the definition of a subsequence, but the 
    // compiler can't handle all these constraints on the associated SubSequence type (yet) 
    if (whereClause == 'SubSequence : ' + API.Sequence + ', SubSequence.' + API.Iterator + '.Element == ' + API.Iterator + '.Element, SubSequence.SubSequence == SubSequence') return kWhereClausePass;
    
    var clauseGroups = whereClause.split(',');
    
    var sameTypeRE = /^\s*([\w.]+) *== *([\w<>.]+)\s*$/;
    var inheritsFromRE = /^\s*([\w.]+) *: *([\w<>.]+)\s*$/;
    
    var matches;
    
    for (var i = 0; i < clauseGroups.length; i++) {
        
        if (matches = clauseGroups[i].match(sameTypeRE)) {
            var left = matches[1].toLowerCase();
            
            // right side - remove generic clause
            var right = removeGenericClause(matches[2]);
            
            // check for self conformance, e.g. extension CollectionType where SubSequence == Self
            if (right == 'Self') {
                right = type;
            }
            
            // console.log('same type');
            
            if (left == API.iterator + '.element') {
                left = 'element';
            }

            // treat all same-type constraints on Element as unknown for now
            // otherwise we lose things like 'where Generator.Element == String'
            if (left == 'element') {
                if (context.types[type].matchedAliases) {
                    var thisElement = context.types[type].matchedAliases.element;
                    
                    // except fail if the matched alias is a tuple type
                    // this is mainly for Dictionary
                    if (!(thisElement) || (thisElement.indexOf('(') == 0)) { return kWhereClauseFail; }
                    
                    // we can also say no IF
                    // this is a concrete type (not a protocol) AND
                    // the Element alias is a known type AND
                    // the Element alias is not equal to the right side
                    if ((context.types[type].kind != 'protocol') &&
                        (context.types[thisElement]) &&
                        (thisElement != right)) { return kWhereClauseFail; }
                }
              
                // otherwise, treat this as unknown  
                return kWhereClauseUnknown;
            }
            
            if (left == 'self') {
                var conf = type == right;
                if (!conf) { return kWhereClauseFail; }

            } else {
                // unknown if we haven't matched any aliases on this type
                if (!context.types[type].matchedAliases) { return kWhereClauseUnknown; }

                // see if we matched the alias referred to on the left already
                var leftType = context.types[type].matchedAliases[left.toLowerCase()];
        
                // unknown if we haven't matched on this constrained type
                if (!leftType) { return kWhereClauseUnknown; }
                
                // remove any generic argument from the type
                leftType = removeGenericClause(leftType);
                                
                var conf = leftType == right;
                if (!conf) { return kWhereClauseFail; }
            }

            continue;
        }
        
        if (matches = clauseGroups[i].match(inheritsFromRE)) {
            var left = matches[1].toLowerCase();

            if (left == API.iterator + '.element') {
                left = 'element';
            }

            if (left == 'self') {
                var conf = checkConformance(context, type, matches[2])

                if (!conf) { return kWhereClauseFail; }
                
            } else {
                // unknown if we haven't matched any aliases on this type
                if (!context.types[type].matchedAliases) { return kWhereClauseUnknown; }

                // see if we matched the alias referred to on the left already
                var leftType = context.types[type].matchedAliases[left.toLowerCase()];
        
                // unknown if we haven't matched on this constrained type
                if (!leftType) { return kWhereClauseUnknown; }
                
                // fail if the matched alias is a tuple type
                // tuples don't conform to any protocols
                if (leftType.indexOf('(') == 0) { return kWhereClauseFail; }
                
                // conformance of a type's generic element is different than its other
                // concrete associated types. If the element doesn't conform to this where
                // clause, don't fail the extension---some instances of the generic type
                // might pass. For example, Set<Element: Hashable> doesn't automatically
                // gain an extension where Element: Comparable, but a Set<Int> will satisfy
                // that requirement.
                var elementConformance = false;

                var extraDebug = false;
                var re = '^(Self.)?(' + API.Iterator + '.)?Element$'
                leftType = leftType.trim();

                if (leftType.match(RegExp(re))) {
                    if (context.types[type].generic && !context.types[type].generic.types) {
                        console.log('------------------\n' + type);
                    }
                    if (context.types[type].generic && context.types[type].generic.types && context.types[type].generic.types.length == 2) {
                        if (context.types[type].generic.types[0] == 'Element') {
                            leftType = context.types[type].generic.types[1];
                            // extraDebug = true;
                            elementConformance = true
                        }
                    }
                }
                
                // remove any generic argument from the type
                leftType = removeGenericClause(leftType);
                if (extraDebug) console.log(leftType);
                
                // unknown if that type doesn't exist explicitly
                if (!context.types[leftType]) { return kWhereClauseUnknown; }
                
                var conf = checkConformance(context, leftType, matches[2])
                if (extraDebug) console.log(matches[2]);
                if (extraDebug) console.log(conf);
                if (!conf) { 
                    if (elementConformance) {
                        return kWhereClauseUnknown;
                    } else {
                        return kWhereClauseFail;                        
                    }
                }
            }
            
            continue;
        }
        
        return kWhereClauseUnknown;
    }
    
    return kWhereClausePass;
}

function attachSingleDefaultImplementation(context, type, proto, item, destContext, kind) {
    var newItem = deepCopy(item);
    newItem.inherited = true;
    
    if (newItem.uniqueSignature && context.types[type].kind != 'protocol') {
        // search for matched aliases and replace the generic protocol versions with our own
        if (context.types[type].matchedAliases) {
            if (context.types[type].matchedAliases['element']) {
                var aliasRegex = RegExp('(Self.|\b)' + API.Iterator + '.Element', 'gi');
                newItem.uniqueSignature = newItem.uniqueSignature.replace(aliasRegex, context.types[type].matchedAliases.element);
                newItem.declaration = newItem.declaration.replace(aliasRegex, context.types[type].matchedAliases.element);
            }
            Object.keys(context.types[type].matchedAliases).forEach(function(matchedAlias) {
                var aliasRegex = RegExp('Self.' + matchedAlias, 'gi');
                newItem.uniqueSignature = newItem.uniqueSignature.replace(aliasRegex, context.types[type].matchedAliases[matchedAlias]);
                newItem.declaration = newItem.declaration.replace(aliasRegex, context.types[type].matchedAliases[matchedAlias]);
            });
        }

        // search for any remaining Self and replace the generic protocol versions with our own
        var nameWithGeneric = context.types[type].name + (context.types[type].generic ? context.types[type].generic.formatted : '');
        var selfRegex = RegExp('\\bSelf\\b', 'g');
        newItem.uniqueSignature = newItem.uniqueSignature.replace(selfRegex, nameWithGeneric);
        newItem.declaration = newItem.declaration.replace(selfRegex, nameWithGeneric);
    }
    
    var index = findWithMatchingUniqueSignature(newItem, destContext[kind]);
    if (index != -1) {
        if (destContext[kind][index].implementationSource) {
            destContext[kind][index].implementationSource.push(proto);
            destContext[kind][index].implementationSource = destContext[kind][index].implementationSource.unique();
        } else {
            destContext[kind][index].implementationSource = [type, proto];
        }
    } else {
        // bail if this property already exists
        if (kind == 'properties') {
            for (var i = 0; i < destContext[kind].length; i++) {
                if (destContext[kind][i].name == newItem.name) {
                    return;
                }
            }
        }
        
        newItem.implementationSource = [proto];
        
        // grab comment from original declaration if needed
        if (newItem.comment == '') {
            var originalIndex = findWithMatchingSignature(newItem, context.types[proto][kind]);
            if (originalIndex != -1) {
                newItem.comment = context.types[proto][kind][originalIndex].comment;
            }
        }
        
        destContext[kind].push(newItem);
    }
}

function attachDefaultImplementations(context, type, protocols) {
    var allInherits = protocols.slice();
    
    if (DEBUG_EXTENSIONS) {
        console.log('#####################################\n' + type ); 
        console.log(context.types[type].matchedAliases);
    }
    
    var i = 0;
    while (i < allInherits.length) {
        var proto = allInherits[i];

        if (context.types[proto] == undefined) { return; }
        if (!context.types[proto].imp) { return; }
        
        if (context.types[type].imp == undefined) { context.types[type].imp = {}; }
        
        Object.keys(context.types[proto].imp).forEach(function(imp) {
            
            var sat = satisfiesWhereClause(context, type, imp);
            if (DEBUG_EXTENSIONS) { 
                var result = sat == kWhereClausePass 
                                    ? 'PASS:    '
                                    : sat == kWhereClauseFail
                                            ? 'FAIL:    '
                                            : 'UNKNOWN: ';
                console.log(result + ': ' + imp); 
            }
            
            // bail if this is a failed where clause
            if (sat == kWhereClauseFail) {
                return;
            }
            
            var subcontext;
            
            if ((sat == kWhereClausePass) && (context.types[type].kind != 'protocol')) {
                // if the where clause passed, atttach default implementations directly to the type itself
                // only for concrete types - protocols need to have all default implementations passed thru
                subcontext = context.types[type];
                
            } else {
                // otherwise there wasn't enough information, so add as pure default implementations
                if (context.types[type].imp[imp] == undefined) {
                    context.types[type].imp[imp] = {
                            conforming: imp,
                            urlprefix: imp.urlify() + '-',
                            operators:  [],
                            functions:  [],
                            types:      {},
                            properties: [],
                            aliases:    [],
                            inits:      [],
                            subscripts: [],
                        };
                }
                subcontext = context.types[type].imp[imp];
            }  
            
            ['functions', 'properties', 'inits', 'subscripts'].forEach(function(group) {
                context.types[proto].imp[imp][group].forEach(function(item) {
                    attachSingleDefaultImplementation(context, type, proto, item, subcontext, group);
                });
                subcontext[group].sort(sortBySignature);
            });
        
        });
        
        // add the next level of inheritance to the list
        context.types[proto].inherits.forEach(function(inherit) {
            if (allInherits.indexOf(inherit) == -1) {
                allInherits.push(inherit);
            }
        });
        
        i++;
    }
}

// ----------------------------------------------------------------------------
// Finalizing steps

function simplifyInheritance(context, type) {
    // make a copy of original list of inherits
    var checklist = context.types[type].inherits.slice();
    var inherits = context.types[type].inherits;

    var i = 0;
    while (i < checklist.length) {
        
        // make sure the type exists
        if (!(context.types[checklist[i]])) {
            i++;
            continue;
        }
        // can't inherit from value types
        if ((context.types[checklist[i]].kind == 'struct') || (context.types[checklist[i]].kind == 'enum')) {
            var index = inherits.indexOf(checklist[i]);
            if (index != -1) {
                inherits.splice(index, 1);
            }
            i++;
            continue;
        }
        
        // add unique inherited symbols to the checklist
        context.types[checklist[i]].inherits.forEach(function(inherit) {
            var index = inherits.indexOf(inherit);
            if (index != -1) {
                inherits.splice(index, 1);
                return;
            }
            
            if (checklist.indexOf(inherit) == -1) {
                checklist.push(inherit);
            }
        });
        
        i++;
    }

    context.types[type].allInherits = checklist.sort();
}

function buildReverseInheritance(context, type) {
    // gradually build a list of types that inherit this type
    context.types[type].inherits.forEach(function(i) {
        if (context.types[i] == undefined) {
            if (DEBUG) console.log('Note: In "' + type + '", can\'t find "' + i + '".');
            return;
        }
        context.types[i].inherited.push(type);
    });
    
    context.types[type].allInherits.forEach(function(i) {
        if (context.types[i] == undefined) {
            if (DEBUG) console.log('Note: In "' + type + '", can\'t find "' + i + '".');
            return;
        }
        context.types[i].allInherited.push(type);
    });
}

function inferGenerator(context, type) {
    var local = context.types[type];
    var generator = '';
    
    var matchedFunction = local.functions.filter(function (f) {
        return (f.name == API.makeIterator);
    } );
                        
    if (matchedFunction.length > 0) {
        generator = matchedFunction[0].ret.line;
    } else {
        var found = findInheritedAlias(context, type, API.Iterator);
        if (found.type) {
            generator = found.type.replace('<Self>', '<' + type + (local.generic ? local.generic.formatted : '') + '>');
        }
    }

    if (!generator) return;
    addFoundAlias(context, type, API.iterator, API.Iterator, generator);
}

function inferElementAndIndex(context, type) {
    var local = context.types[type];
    var element = '';
    var index = '';
    
    var matchedSubscript = local.subscripts.filter(function (s) {
        return (!s.params[0].type.match(/\bRange\b/)) && (!s.ret.line.match(/\?$/));
    });
    if (matchedSubscript.length > 0) {
        element = matchedSubscript[0].ret.line.replace(/ *\{[^\}]+\}/, '').trim();
        index = matchedSubscript[0].params[0].type;
    } else {
        if ((local.generic && local.generic.types && local.generic.types.length == 2) &&
            (local.generic.types[0] == 'Element')) 
        {
            element = local.generic.types[1];
        } else {
            // default element
            element = API.Iterator + '.Element';
        }
    }

    if (element)
        addFoundAlias(context, type, 'element', 'Element', element);
    if (index)
        addFoundAlias(context, type, 'index', 'Index', index);
}

function inferSubsequence(context, type) {
    var local = context.types[type];
    var subsequence = '';
    
    var matchedSubscript = local.subscripts.filter(function (s) {
        return (s.params[0].type.match(/\bRange\b/)) && (!s.ret.line.match(/\?$/));
    });
    if (matchedSubscript.length > 0) {
        subsequence = matchedSubscript[0].ret.line.replace(/ *\{[^\}]+\}/, '').trim();
    } else {
        var found = findInheritedAlias(context, type, 'SubSequence');
        var localType = type + (local.generic ? local.generic.formatted : '');
        if (found.type) {
            subsequence = found.type.replace('<Self>', '<' + localType + '>');
        } else {
            if (checkConformance(context, type, API.Collection)) {
                subsequence = 'Slice<' + localType + '>';    
            }
        }
    }

    if (subsequence)
        addFoundAlias(context, type, 'subsequence', 'SubSequence', subsequence);
}

function addFoundAlias(context, type, aliasKey, aliasName, aliasType) {
    // don't overwrite existing
    if (context.types[type].matchedAliases[aliasKey]) {
        console.log(type + ': ' + aliasName + ' overwrite skipped. new:' + aliasType +
        ', old:' + context.types[type].matchedAliases[aliasKey]);
        return;
    }
    
    context.types[type].matchedAliases[aliasKey] = aliasType;
    context.types[type].aliases.push( {
        kind:       'typealias',
        name:       aliasName,
        type:       aliasType,
        proto:      '',
        comment:    '*Type alias inferred.*'
    } );
}

function matchExistingAlias(context, type, aliasKey, aliasName) {
    var local = context.types[type];
    local.aliases.forEach(function(alias) {
        if (alias.name == aliasName) {
            local.matchedAliases[aliasKey] = alias.type;
        }
    });
    if (local.matchedAliases[aliasKey]) return true;
    return false;
}

function inferAssociatedTypes(context, type) {
    var local = context.types[type];
    if (local.kind == 'protocol') { return; }
    local.matchedAliases = {};

    var isSequence = checkConformance(context, type, API.Sequence);
    var isCollection = checkConformance(context, type, API.Collection);
//1console.log(type + ': ' + isSequence + ', ' + isCollection);

    if (isSequence) {
        if (!matchExistingAlias(context, type, 'generator', API.Iterator)) {
            inferGenerator(context, type);
        }
        if (!matchExistingAlias(context, type, 'element', 'Element') ||
            !matchExistingAlias(context, type, 'index', 'Index')) {
            inferElementAndIndex(context, type);
        }
        if (!matchExistingAlias(context, type, 'subsequence', 'SubSequence')) {
            inferSubsequence(context, type);
        }
    }
//1console.log(local.matchedAliases);
}

function determineSignatures(context, type) {
    // finalize initializers and functions
    // "finalize" in this case means to create signature, uniqueSignature, and declaration properties for
    // use by SwiftDoc.org, then sort the lists
    context.types[type].inits = finalizeItems(context.types[type].inits, signatureForInit, declarationForInit);
    context.types[type].functions = finalizeItems(context.types[type].functions, signatureForFunction, declarationForFunction);
    // finalize properties
    context.types[type].properties = finalizeProperties(context.types[type].properties);
    context.types[type].subscripts = finalizeSubscripts(context.types[type].subscripts);
    
    // next do the same finalizing and sorting within default implementations
    if (!context.types[type].imp) { return; }
    Object.keys(context.types[type].imp).forEach(function(i) {
        context.types[type].imp[i].inits = finalizeItems(context.types[type].imp[i].inits, signatureForInit, declarationForInit);
        context.types[type].imp[i].functions = finalizeItems(context.types[type].imp[i].functions, signatureForFunction, declarationForFunction);
        
        context.types[type].imp[i].properties = finalizeProperties(context.types[type].imp[i].properties);
        context.types[type].imp[i].subscripts = finalizeSubscripts(context.types[type].imp[i].subscripts);
    });
    
    // for protocols, look through the unrestricted default implementation and mark functions and properties
    // as whether a default implementation exists.
    if (context.types[type].kind == 'protocol') {
        if (context.types[type].imp['*'] == undefined) { return; }
        
        context.types[type].functions.forEach(function(f) {
            // special case for Comparable default implementations
            if ((f.name == '>') || (f.name == '>=') || (f.name == '<=')) {
                f.defaultImplementation = true;
                return;
            }
            // no default implementations at all?
            if (context.types[type].imp['*'] == undefined) { 
                f.defaultImplementation = false;
                return; 
            }
            f.defaultImplementation = containsWithMatchingSignature(f, context.types[type].imp['*'].functions);
        });
        context.types[type].properties.forEach(function(p) {
            if (context.types[type].imp['*'] == undefined) { 
                p.defaultImplementation = false;
                return; 
            }
            p.defaultImplementation = containsWithMatchingSignature(p, context.types[type].imp['*'].properties);
        });
    } 
}

function markSelfRequirements(context, type) {
    if (context.types[type].kind != 'protocol') return;
    
    var prot = context.types[type];
    prot.requiresSelf = false;
    
    // do we have any associated types?
    if (prot.aliases.length > 0) {
        prot.requiresSelf = true;
        return;
    }
    
    // this is going to be ugly - look for Self in parames & return values of everything
    ['functions', 'inits', 'subscripts'].forEach(function(group) {
        prot[group].forEach(function(f) {
            f.params.forEach(function(p) {
                if (p.types && (p.types.indexOf('Self') != -1)) {
                    prot.requiresSelf = true;
                }
            });
            if (f.ret && f.ret.types && (f.ret.types.indexOf('Self') != -1)) {
                prot.requiresSelf = true;
            }
        });
    });
    
    // check properties too
    prot.properties.forEach(function(p) {
        if (p.subtypes.indexOf('Self') != -1) {
            prot.requiresSelf = true;
        }
    });
}

function markDefaultImplementations(context, type) {
    if (context.types[type].kind != 'protocol') return;
    
    ['functions', 'properties', 'inits', 'subscripts'].forEach(function(group) {
        context.types[type][group].forEach(function(f) {
            // special case for Comparable default implementations
            if ((f.name == '>') || (f.name == '>=') || (f.name == '<=')) {
                f.defaultImplementation = true;
                return;
            }
            // no default implementations at all?
            if (context.types[type].imp['*'] == undefined) {
                f.defaultImplementation = false;
                return; 
            }
            f.defaultImplementation = containsWithMatchingSignature(f, context.types[type].imp['*'][group]);
        });
    });
}

function addDefaultImplementations(context, type) {
    attachDefaultImplementations(context, type, context.types[type].inherits);
    context.types[type].functions.sort(sortBySignature);
}

function addProtocolInheritance(context, type) {
    if (context.types[type].kind != 'protocol') return;

    context.types[type].allInherited.forEach(function(inheritingType) {
        if (context.types[inheritingType].kind != 'protocol') return;
        
        ['functions', 'properties', 'inits', 'subscripts', 'aliases'].forEach(function(group) {
            context.types[type][group].forEach(function(f) {
                if (f.inherited) return;
                attachSingleDefaultImplementation(context, inheritingType, type, f, context.types[inheritingType], group);
            });
        });
    });
}

function raiseOperators(context, type) {
    // for any type that isn't a protocol, move any operator functions up
    // to the top level list of functions
    
    if (context.types[type].kind == 'protocol') return;
    
    var op_funcs = context.types[type].functions.filter(function(f) {
        return (f.kind == 'operator func');
    });
    context.functions = context.functions.extend(op_funcs);
        
    context.types[type].functions = context.types[type].functions.filter(function(f) {
        return (f.kind == 'func');
    });
}

function finalizeOperators(context) {
    // add signatures for top-level functions
    context.functions.forEach(function (item) {
        item.signature = signatureForFunction(item, false);
        item.uniqueSignature = signatureForFunction(item, true);
        item.declaration = declarationForFunction(item);
        item.uniqueSignatureURL = signatureForFunction(item, true, true).urlify();
        item.declarationURL = item.declaration.urlify();
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
    
    // strip operator functions from top-level functions and sort by name
    context.functions = context.functions.filter(function(f) {
        return (f.kind == 'func');
    }).sort(function(a, b) {
        if (a.line.length != b.line.length) {
            return a.line.length - b.line.length;
        }
        return String.naturalCompare(a.line, b.line);
    });
    
    // sort operators by place and name
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
}

function finalize(context) {
    var forEachWithContext = function(array, func) {
        array.forEach(function (item) {
            func(context, item);
        });
    }
    
    // bring nested types up to the top level, prepending parent type's name
    console.log('Hoisting nested types');
    hoistNestedTypes(context.types);

    // simplify the inheritance graphs for all types & protocols
    console.log('Simplifying inheritance');
    forEachWithContext(Object.keys(context.types), simplifyInheritance);
    
    // types and protocols have a full list of protocols they inherit
    // time to build the reverse list, of inherited protocols and types for each protocol
    console.log('Building reverse inheritance');
    forEachWithContext(Object.keys(context.types), buildReverseInheritance);
    
    // build out the type aliases for types where they would be inferred by the compiler
    // for example, Array doesn't explicitly declare Int as its Index type, but that 
    // can be inferred from the subscript methods that are declared. 
    console.log('Inferring type aliases');
    forEachWithContext(Object.keys(context.types), inferAssociatedTypes);

    // create signature, uniqueSignature, and declaration properties for initializers,
    // functions, and properties for use by SwiftDoc.org, then sort the lists
    console.log('Creating method signatures');
    forEachWithContext(Object.keys(context.types), determineSignatures);
    
    // for protocols, copy declarations to other inheriting protocols
    console.log('Inheriting protocol requirements');
    forEachWithContext(Object.keys(context.types), addProtocolInheritance);

    // now attach default implementations to types
    console.log('Attaching default implementations to concrete types');
    forEachWithContext(Object.keys(context.types), addDefaultImplementations);
    
    // for protocols, look through the unrestricted default implementation and mark functions and properties
    // as whether a default implementation exists.
    console.log('Marking default implementations');
    forEachWithContext(Object.keys(context.types), markDefaultImplementations);

    // for protocols, check for Self or associated type requirements
    console.log('Simplifying associated type descriptors');
    forEachWithContext(Object.keys(context.types), markSelfRequirements);

    // regroup and sort functions, operators, and operator functions
    console.log('Finalizing operators');
    forEachWithContext(Object.keys(context.types), raiseOperators);
    finalizeOperators(context);
    
    return context;
}

// ----------------------------------------------------------------------------
// Export

var Parser = function() { };

Parser.prototype.parse = function(data, context) {
    // pre-parsing data cleanup
    data = data.replace(/public\s*/g, '')
                .replace(/\@warn_unused_result(\([^)]+\))?\s*/g, '')
                .replace(/\@rethrows\s*/g, '')
                .replace(/__consuming/g, '')
                .replace(/^([ \t]*)\@available\(\*, deprecated, message[:= ]+"([^"]+)"\)/gm,
                        '$1///\n$1/// *Deprecated:* $2.')
                .replace('@warn_unqualified_access', '')
                .replace(/extension Set<Element>.Index where Element : Hashable/, 'extension Set.Index')
                // put where clauses back in angle brackets
//                 .replace(/^(.+?)<([^<>]+)>(.*)( where .+?)( ?{ *)?$/gm, '$1<$2$4>$3$5')
                // fix funny dictionary extension syntax
                .replace(/\[Key *: *Value\].Index/g, 'Dictionary.Index')
                // fix funny set extension syntax
                .replace(/extension +Set\<Element where Element +: Hashable\>/g, 'extension Set')
                ;
    console.log("Finished pre-parsing")
    fs.writeFileSync('preparse-data.txt', data);

    // initialize context for parsing
    var parsedData = context || {
        operators: [],
        functions: [],
        properties: [],
        aliases: [],
        precedencegroups: [],
        types: {},
    }
    
    // parse all the things
    parse(parsedData, data);
    
    // clear any unused comments 
    currentComment = [];
        
    return parsedData;
}

Parser.prototype.useSwift3APIs = function() {
    API = { 
        iterator: 'iterator',
        Iterator: 'Iterator', 
        makeIterator: 'makeIterator', 
        Sequence: 'Sequence', 
        Collection: 'Collection',
    };
}

Parser.prototype.finalize = finalize;

module.exports = new Parser();

