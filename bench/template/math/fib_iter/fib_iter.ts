function fib(n: number): number {
    let a = 0, b = 1;
    for (let i = 0; i < n; i++) {
        const tmp = a + b;
        a = b;
        b = tmp;
    }
    return a;
}

const n = {{ .N }};
const repeat = 1000;
let last = 0;

const start = performance.now();
for (let i = 0; i < repeat; i++) {
    last = fib(n);
}
const duration = performance.now() - start;

const output = {
    duration_ms: duration,
    output: last,
};

console.log(JSON.stringify(output));
