// Generated by Mochi compiler v0.10.27 on 1970-01-01T00:00:00Z
// Source: /workspace/mochi/tests/vm/valid/string_compare.mochi

function main(): void {
  console.log(("a" < "b") ? 1 : 0);
  console.log(("a" <= "a") ? 1 : 0);
  console.log(("b" > "a") ? 1 : 0);
  console.log(("b" >= "b") ? 1 : 0);
}
main();
