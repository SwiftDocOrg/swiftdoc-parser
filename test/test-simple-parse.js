// includes
var fs = require('fs'),
    expect = require('chai').expect,
    parser = require('../lib/swiftdoc-parser');

var dataFilename = 'test/data/simple.txt';
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

        it('count', function() {
            expect(parsedData.operators.length).to.equal(2);
        });
        
        it('+ functions', function() {
            expect(plus.functions.length).to.equal(1);
        });
        
    });
    
    describe('types + protocols', function() {
        
        it('count', function() {
            // Int, FixedWidthInteger, UnsafeMutableBufferPointer
            expect(Object.keys(parsedData.types).length).to.equal(3);
        });
        
        describe('`Int`', function() {
        
            var intData = parsedData.types['Int'];
            
            it('type', function() {
                expect(intData.kind).to.equal('struct');
            });

            it('init count', function() {
                expect(intData.inits.length).to.equal(3);
            });
        });        
    });
    
});
