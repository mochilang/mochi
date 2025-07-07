const a = [1,2];
const b = [2,3];
const union = Array.from(new Set([...a, ...b]));
console.log(union.join(' '));
const except = [1,2,3].filter(x => ![2].includes(x));
console.log(except.join(' '));
const intersect = [1,2,3].filter(x => [2,4].includes(x));
console.log(intersect.join(' '));
console.log([...a, ...b].length);
