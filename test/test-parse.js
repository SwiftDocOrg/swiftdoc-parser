// includes
var fs = require('fs'),
    expect = require('chai').expect,
    parser = require('../lib/swiftdoc-parser');

// 'data/swift-lang-6.2b1.txt'
var dataFilename = 'test/data/ok.txt';
var data = fs.readFileSync(dataFilename, 'utf8');
var parsedData = parser.parse(data, null);
parsedData = parser.finalize(parsedData);

Array.prototype.firstElementNamed = function(name) {
    var result = this.filter(function(e) { return e.name == name });
    return result.length == 0 ? null : result[0];
}

describe('Parser', function() {
    describe('operators', function() {

        var plus        = parsedData.operators[0];
        var equality    = parsedData.operators[1];
        var increment   = parsedData.operators[2];

        it('count', function() {
            expect(parsedData.operators.length).to.equal(3);
        });
        
        it('+ functions', function() {
            expect(plus.functions.length).to.equal(4);
        });
        
        it('names', function() {
            expect(plus.name).to.equal('+');
            expect(increment.name).to.equal('prefix ++');
            expect(equality.name).to.equal('==');
        });
        
    });
    
    describe('type aliases', function() {
    
        it('count', function() {
            expect(parsedData.aliases.length).to.equal(7);
        });
    
        it('typing', function() {
            expect(parsedData.aliases.firstElementNamed('CLong').type).to.equal('Int');
        });
        
    });
    
    describe('properties', function() {
        
        it('count', function() {
            expect(parsedData.properties.length).to.equal(1);
        });
        
    });

    describe('functions', function() {
    
        it('count', function() {
            expect(parsedData.functions.length).to.equal(11);
        });
    
        var partitionFunction = parsedData.functions.firstElementNamed('partition');
        it('`partition` parameters', function() {
            expect(partitionFunction).to.exist;

            expect(partitionFunction.params.length).to.equal(3);

            expect(partitionFunction.params[2].name).to.equal('isOrderedBefore');
            expect(partitionFunction.params[2].types).to.contain('Bool');
            expect(partitionFunction.params[2].types).to.contain('Element');
            expect(partitionFunction.params[2].types).to.contain('Generator');
        });
        
        var withUnsafeFunction = parsedData.functions.firstElementNamed('withUnsafeMutablePointers');
        it('`withUnsafeMutablePointers` function', function() {
            expect(withUnsafeFunction).to.exist;

            expect(withUnsafeFunction.generic.line).to.equal('A0, A1, A2, Result');
            expect(withUnsafeFunction.params.length).to.equal(4);
            expect(withUnsafeFunction.params[3].type).to.equal('(UnsafeMutablePointer<A0>, UnsafeMutablePointer<A1>, UnsafeMutablePointer<A2>) -> Result');
        });
    });

    describe('types + protocols', function() {
        
        it('count', function() {
            expect(Object.keys(parsedData.types).length).to.equal(41);
        });
        
        describe('`Array`', function() {
        
            var arrayData = parsedData.types['Array'];
            
            it('type', function() {
                expect(arrayData.kind).to.equal('struct');
            });
            
            it('init count', function() {
                expect(arrayData.inits.length).to.equal(5);
            });
        
            it('property count', function() {
                expect(arrayData.properties.length).to.equal(7);
            });
        
            it('function count', function() {
                expect(arrayData.functions.length).to.equal(15);
            });
        
            it('subscript count', function() {
                expect(arrayData.subscripts.length).to.equal(2);
            });
        
            it('alias count', function() {
                expect(arrayData.aliases.length).to.equal(2);
            });
        
            it('inheritance', function() {
                expect(arrayData.inherits).to.contain('ArrayLiteralConvertible');
                expect(arrayData.inherits).to.contain('MutableCollectionType');
                expect(arrayData.inherits).to.contain('Sliceable');
            });
            
            it('comments', function() {
                expect(arrayData.comment).to.contain('is deferred until the\nfirst element access');
                expect(arrayData.comment).to.contain('  var b = a');
            });
        });
        
        describe('`_CollectionType`', function() {

            var collectionTypeData = parsedData.types['_CollectionType'];
            
            it('type', function() {
                expect(collectionTypeData.kind).to.equal('protocol');
            });
            
            it('init count', function() {
                expect(collectionTypeData.inits.length).to.equal(0);
            });
        
            it('property count', function() {
                expect(collectionTypeData.properties.length).to.equal(2);
            });
        
            it('function count', function() {
                expect(collectionTypeData.functions.length).to.equal(0);
            });
        
            it('subscript count', function() {
                expect(collectionTypeData.subscripts.length).to.equal(1);
            });
        
            it('alias count', function() {
                expect(collectionTypeData.aliases.length).to.equal(2);
            });
        
            it('inheritance', function() {
                expect(collectionTypeData.inherits).to.contain('_SequenceType');
                expect(collectionTypeData.inherited).to.contain('CollectionType');
            });
            
        });
        
        
    });
});
