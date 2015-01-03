
// get substring of string, using start and end indices from a two-element array parameter
// useful for getting the matched substring from PCRE match result
String.prototype.ss = function (indices) {
    return this.substring(indices[0], indices[1]);
};

// returns a new array with only unique elements of the array
Array.prototype.unique = function() {
    return this.reduce(function(p, c) { 
                    if (p.indexOf(c) < 0) p.push(c);
                    return p;
                }, []);
}

// returns a new array with the elements of `arr` appended to the array
Array.prototype.extend = function(arr) {
    this.push.apply(this, arr);
    return this;
}

