function sum(n: number): number {
    let total = 0;
    for (let i = 1; i <= n; i++) {
        total += i;
    }
    return total;
}

const n = {{ .N }};
const repeat = 1000;
let last = 0;

const start = performance.now();
for (let i = 0; i < repeat; i++) {
    last = sum(n);
}
const duration = performance.now() - start;

console.log(JSON.stringify({
    duration_ms: duration,
    output: last,
}));
