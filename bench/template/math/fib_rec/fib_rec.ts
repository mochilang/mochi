function fib(n: number): number {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

const n = {{ .N }};
const start = performance.now();
const result = fib(n);
const duration = performance.now() - start;

const output = {
    duration_ms: duration,
    output: result,
};

console.log(JSON.stringify(output));
