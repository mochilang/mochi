// Generated by Mochi compiler v0.10.27 on 1970-01-01T00:00:00Z
// Source: /workspace/mochi/tests/vm/valid/map_assign.mochi

var scores: { [key: string]: number };

function main(): void {
  scores = { "alice": 1 };
  scores["bob"] = 2;
  console.log(scores["bob"]);
}
main();
