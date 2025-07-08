let xs = [1, 2, 3];
console.log((Array.isArray(xs) || typeof xs === 'string' ? xs.includes(2) : Object.prototype.hasOwnProperty.call(xs, 2)));
console.log((!((Array.isArray(xs) || typeof xs === 'string' ? xs.includes(5) : Object.prototype.hasOwnProperty.call(xs, 5)))));
