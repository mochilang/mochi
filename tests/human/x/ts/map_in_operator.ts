const m: Record<number, string> = { 1: "a", 2: "b" };
console.log(Object.prototype.hasOwnProperty.call(m, 1));
console.log(Object.prototype.hasOwnProperty.call(m, 3));
