// Generated by Mochi v0.10.41 on 2025-07-26 17:31:26 GMT+7

function isInt(s: string): boolean {
  if (((Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length) == 0)) {
    return false;
  }
  for (const ch of s) {
    if (((ch < "0") || (ch > "9"))) {
      return false;
    }
  }
  return true;
}
function main() {
  console.log("Are these strings integers?");
  let v: string = "1";
  let b: boolean = false;
  if (isInt(v)) {
    b = true;
  }
  console.log(((("  " + v) + " -> ") + String(b)));
  let i: string = "one";
  console.log(((("  " + i) + " -> ") + String(isInt(i))));
}
main();
