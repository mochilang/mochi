function matmul(a: number[][], b: number[][]): number[][] {
    const n = a.length, m = b[0].length, p = b.length;
    const result: number[][] = [];

    for (let i = 0; i < n; i++) {
        const row: number[] = [];
        for (let j = 0; j < m; j++) {
            let sum = 0;
            for (let k = 0; k < p; k++) {
                sum += a[i][k] * b[k][j];
            }
            row.push(sum);
        }
        result.push(row);
    }
    return result;
}

const size = {{ .N }};
const repeat = 10;

const a = Array.from({ length: size }, (_, i) =>
    Array.from({ length: size }, (_, j) => i + j)
);
const b = Array.from({ length: size }, (_, i) =>
    Array.from({ length: size }, (_, j) => i * j)
);

let last: number[][] = [];
const start = performance.now();
for (let i = 0; i < repeat; i++) {
    last = matmul(a, b);
}
const duration = performance.now() - start;

console.log(JSON.stringify({
    duration_ms: duration,
    output: last[0][0],
}));
