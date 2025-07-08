let m = {a: 1, b: 2};
console.log((Array.isArray(m) || typeof m === 'string' ? m.includes("a") : Object.prototype.hasOwnProperty.call(m, "a")));
console.log((Array.isArray(m) || typeof m === 'string' ? m.includes("c") : Object.prototype.hasOwnProperty.call(m, "c")));
