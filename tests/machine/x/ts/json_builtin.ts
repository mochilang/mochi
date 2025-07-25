// Generated by Mochi compiler v0.10.27 on 1970-01-01T00:00:00Z
// Source: /workspace/mochi/tests/vm/valid/json_builtin.mochi

let m: { [key: string]: number };

function main(): void {
  m = {
    "a": 1,
    "b": 2,
  };
  console.log(_json(m));
}
function _json(v: any): string {
  function _sort(x: any): any {
    if (Array.isArray(x)) return x.map(_sort);
    if (x && typeof x === "object") {
      const keys = Object.keys(x).sort();
      const o: any = {};
      for (const k of keys) o[k] = _sort(x[k]);
      return o;
    }
    return x;
  }
  return JSON.stringify(_sort(v));
}

main();
