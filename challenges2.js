function memoize(fn) {
    var mem = {};
    return function (a) {
        var cachedA = mem[a];
        if (cachedA) {
            console.log("cache hit");
            return cachedA;
        }
        else {
            var b = fn(a);
            mem[a] = b;
            return b;
        }
    };
}
var memoizedParsedInt = memoize(parseInt);
memoizedParsedInt("42");
memoizedParsedInt("42");
