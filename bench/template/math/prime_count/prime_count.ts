function isPrime(n: number): boolean {
    if (n < 2) return false;
    for (let i = 2; i < n; i++) {
        if (n % i === 0) return false;
    }
    return true;
}

const n = {{ .N }};
const repeat = 100;
let last = 0;

const start = performance.now();
for (let r = 0; r < repeat; r++) {
    let count = 0;
    for (let i = 2; i <= n; i++) {
        if (isPrime(i)) count++;
    }
    last = count;
}
const duration = performance.now() - start;

const output = {
    duration_ms: duration,
    output: last,
};

console.log(JSON.stringify(output));
