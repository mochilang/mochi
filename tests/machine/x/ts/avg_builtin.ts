// Generated by Mochi compiler v0.10.27 on 1970-01-01T00:00:00Z
// Source: /workspace/mochi/tests/vm/valid/avg_builtin.mochi

function main(): void {
  console.log([1, 2, 3].reduce((a, b) => a + Number(b), 0) / [1, 2, 3].length);
}
main();
