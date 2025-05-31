function fact(n: number): number {
    if (n === 0) return 1;
    return n * fact(n - 1);
}

const n = {{ .N }};
const repeat = 1000;
let last = 0;

const start = performance.now();
for (let i = 0; i < repeat; i++) {
    last = fact(n);
}
const duration = performance.now() - start;

console.log(JSON.stringify({
    duration_ms: duration,
    output: last,
}));
