let m = {1: "a", 2: "b"};
console.log((Array.isArray(m) || typeof m === 'string' ? m.includes(1) : Object.prototype.hasOwnProperty.call(m, 1)));
console.log((Array.isArray(m) || typeof m === 'string' ? m.includes(3) : Object.prototype.hasOwnProperty.call(m, 3)));
