// Generated by Mochi compiler v0.10.27 on 1970-01-01T00:00:00Z
// Source: /workspace/mochi/tests/vm/valid/in_operator_extended.mochi

let m: { [key: string]: number };
let s: string;
let xs: number[];
let ys: number[];

function main(): void {
  xs = [1, 2, 3];
  ys = xs.filter((x) => ((x % 2) == 1)).map((x) => x);
  console.log((ys.includes(1) ? 1 : 0) ? 1 : 0);
  console.log((ys.includes(2) ? 1 : 0) ? 1 : 0);
  m = { "a": 1 };
  console.log(
    (Object.prototype.hasOwnProperty.call(m, String("a")) ? 1 : 0) ? 1 : 0,
  );
  console.log(
    (Object.prototype.hasOwnProperty.call(m, String("b")) ? 1 : 0) ? 1 : 0,
  );
  s = "hello";
  console.log((s.includes("ell") ? 1 : 0) ? 1 : 0);
  console.log((s.includes("foo") ? 1 : 0) ? 1 : 0);
}
main();
