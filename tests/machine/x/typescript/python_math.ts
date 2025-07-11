const math = { pi: Math.PI, e: Math.E, sqrt: Math.sqrt, pow: Math.pow, sin: Math.sin, log: Math.log };

const r = 3;
const area = (math.pi * math.pow(r, 2));
const root = math.sqrt(49);
const sin45 = math.sin((math.pi / 4));
const log_e = math.log(math.e);
console.log("Circle area with r =", r, "=>", area);
console.log("Square root of 49:", root);
console.log("sin(π/4):", sin45);
console.log("log(e):", log_e);
