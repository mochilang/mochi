function mul(n: number): number {
    let result = 1;
    for (let i = 1; i <= n; i++) {
        result *= i;
    }
    return result;
}

const n = {{ .N }};
const repeat = 1000;
let last = 0;

const start = performance.now();
for (let i = 0; i < repeat; i++) {
    last = mul(n);
}
const duration = performance.now() - start;

const output = {
    duration_ms: duration,
    output: last,
};

console.log(JSON.stringify(output));
