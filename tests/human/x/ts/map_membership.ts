const m = { a: 1, b: 2 };
console.log(Object.prototype.hasOwnProperty.call(m, "a"));
console.log(Object.prototype.hasOwnProperty.call(m, "c"));
