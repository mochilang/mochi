let s = "catch";
console.log((Array.isArray(s) || typeof s === 'string' ? s.includes("cat") : Object.prototype.hasOwnProperty.call(s, "cat")));
console.log((Array.isArray(s) || typeof s === 'string' ? s.includes("dog") : Object.prototype.hasOwnProperty.call(s, "dog")));
