var f = function (x) {
    return x * x;
};
var g = function (x) {
    var h = function () { return z * z; };
    var a = h();
    var z = x * x;
    return a;
};
console.log(g(2));
