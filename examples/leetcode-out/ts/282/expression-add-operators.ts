// Generated by Mochi TypeScript compiler

function parseInt(s: string): number {
  let result: number = 0;
  (globalThis as any).result = result;
  let i: number = 0;
  (globalThis as any).i = i;
  let digits: Record<string, number> = {
    "0": 0,
    "1": 1,
    "2": 2,
    "3": 3,
    "4": 4,
    "5": 5,
    "6": 6,
    "7": 7,
    "8": 8,
    "9": 9,
  };
  (globalThis as any).digits = digits;
  while ((i < s.length)) {
    result = (result * 10) + digits[_indexString(s, i)];
    i = i + 1;
  }
  return result;
}

function addOperators(num: string, target: number): Array<string> {
  let result: Array<string> = [];
  (globalThis as any).result = result;
  function backtrack(
    pos: number,
    expr: string,
    value: number,
    prev: number,
  ): void {
    if ((pos == num.length)) {
      if ((value == target)) {
        result = result.concat([expr]);
      }
    } else {
      let i: number = pos;
      (globalThis as any).i = i;
      while ((i < num.length)) {
        if (((i != pos) && (_indexString(num, pos) == "0"))) {
          break;
        }
        let part: string = _sliceString(num, pos, i + 1);
        (globalThis as any).part = part;
        let cur: number = parseInt(part);
        (globalThis as any).cur = cur;
        if ((pos == 0)) {
          backtrack(i + 1, part, cur, cur);
        } else {
          backtrack(i + 1, expr + "+" + part, value + cur, cur);
          backtrack(i + 1, expr + "-" + part, value - cur, -cur);
          backtrack(
            i + 1,
            expr + "*" + part,
            (value - prev) + (prev * cur),
            prev * cur,
          );
        }
        i = i + 1;
      }
    }
  }
  backtrack(0, "", 0, 0);
  return result;
}

function test_example_1(): void {
  let res: Array<string> = addOperators("123", 6);
  (globalThis as any).res = res;
  let sorted: Array<string> = (() => {
    const _src = res;
    let _items = [];
    for (const x of _src) {
      _items.push(x);
    }
    let _pairs = _items.map((it) => {
      const x = it;
      return { item: it, key: x };
    });
    _pairs.sort((a, b) => {
      const ak = a.key;
      const bk = b.key;
      if (typeof ak === "number" && typeof bk === "number") return ak - bk;
      if (typeof ak === "string" && typeof bk === "string") {
        return ak < bk
          ? -1
          : (ak > bk ? 1 : 0);
      }
      return String(ak) < String(bk) ? -1 : (String(ak) > String(bk) ? 1 : 0);
    });
    _items = _pairs.map((p) => p.item);
    const _res = [];
    for (const x of _items) {
      _res.push(x);
    }
    return _res;
  })();
  (globalThis as any).sorted = sorted;
  if (
    !(_equal(sorted, [
      "1*2*3",
      "1+2+3",
    ]))
  ) throw new Error("expect failed");
}

function test_example_2(): void {
  let res: Array<string> = addOperators("232", 8);
  (globalThis as any).res = res;
  let sorted: Array<string> = (() => {
    const _src = res;
    let _items = [];
    for (const x of _src) {
      _items.push(x);
    }
    let _pairs = _items.map((it) => {
      const x = it;
      return { item: it, key: x };
    });
    _pairs.sort((a, b) => {
      const ak = a.key;
      const bk = b.key;
      if (typeof ak === "number" && typeof bk === "number") return ak - bk;
      if (typeof ak === "string" && typeof bk === "string") {
        return ak < bk
          ? -1
          : (ak > bk ? 1 : 0);
      }
      return String(ak) < String(bk) ? -1 : (String(ak) > String(bk) ? 1 : 0);
    });
    _items = _pairs.map((p) => p.item);
    const _res = [];
    for (const x of _items) {
      _res.push(x);
    }
    return _res;
  })();
  (globalThis as any).sorted = sorted;
  if (
    !(_equal(sorted, [
      "2*3+2",
      "2+3*2",
    ]))
  ) throw new Error("expect failed");
}

function test_example_3(): void {
  let res: Array<string> = addOperators("105", 5);
  (globalThis as any).res = res;
  let sorted: Array<string> = (() => {
    const _src = res;
    let _items = [];
    for (const x of _src) {
      _items.push(x);
    }
    let _pairs = _items.map((it) => {
      const x = it;
      return { item: it, key: x };
    });
    _pairs.sort((a, b) => {
      const ak = a.key;
      const bk = b.key;
      if (typeof ak === "number" && typeof bk === "number") return ak - bk;
      if (typeof ak === "string" && typeof bk === "string") {
        return ak < bk
          ? -1
          : (ak > bk ? 1 : 0);
      }
      return String(ak) < String(bk) ? -1 : (String(ak) > String(bk) ? 1 : 0);
    });
    _items = _pairs.map((p) => p.item);
    const _res = [];
    for (const x of _items) {
      _res.push(x);
    }
    return _res;
  })();
  (globalThis as any).sorted = sorted;
  if (
    !(_equal(sorted, [
      "1*0+5",
      "10-5",
    ]))
  ) throw new Error("expect failed");
}

function test_example_4(): void {
  let res: Array<string> = addOperators("00", 0);
  (globalThis as any).res = res;
  let sorted: Array<string> = (() => {
    const _src = res;
    let _items = [];
    for (const x of _src) {
      _items.push(x);
    }
    let _pairs = _items.map((it) => {
      const x = it;
      return { item: it, key: x };
    });
    _pairs.sort((a, b) => {
      const ak = a.key;
      const bk = b.key;
      if (typeof ak === "number" && typeof bk === "number") return ak - bk;
      if (typeof ak === "string" && typeof bk === "string") {
        return ak < bk
          ? -1
          : (ak > bk ? 1 : 0);
      }
      return String(ak) < String(bk) ? -1 : (String(ak) > String(bk) ? 1 : 0);
    });
    _items = _pairs.map((p) => p.item);
    const _res = [];
    for (const x of _items) {
      _res.push(x);
    }
    return _res;
  })();
  (globalThis as any).sorted = sorted;
  if (
    !(_equal(sorted, [
      "0*0",
      "0+0",
      "0-0",
    ]))
  ) throw new Error("expect failed");
}

function test_example_5(): void {
  if (!(_equal(addOperators("3456237490", 9191), []))) {
    throw new Error("expect failed");
  }
}

function test_single_number(): void {
  if (!(_equal(addOperators("5", 5), ["5"]))) throw new Error("expect failed");
}

function main(): void {
  test_example_1();
  test_example_2();
  test_example_3();
  test_example_4();
  test_example_5();
  test_single_number();
}
function _equal(a: any, b: any): boolean {
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) if (!_equal(a[i], b[i])) return false;
    return true;
  }
  if (a && b && typeof a === "object" && typeof b === "object") {
    const ak = Object.keys(a);
    const bk = Object.keys(b);
    if (ak.length !== bk.length) return false;
    for (const k of ak) {
      if (!bk.includes(k) || !_equal((a as any)[k], (b as any)[k])) {
        return false;
      }
    }
    return true;
  }
  return a === b;
}

function _indexString(s: string, i: number): string {
  const runes = Array.from(s);
  if (i < 0) i += runes.length;
  if (i < 0 || i >= runes.length) throw new Error("index out of range");
  return runes[i];
}

function _sliceString(s: string, i: number, j: number): string {
  let start = i;
  let end = j;
  const runes = Array.from(s);
  const n = runes.length;
  if (start < 0) start += n;
  if (end < 0) end += n;
  if (start < 0) start = 0;
  if (end > n) end = n;
  if (end < start) end = start;
  return runes.slice(start, end).join("");
}

main();
