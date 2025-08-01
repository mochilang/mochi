// Generated by Mochi TypeScript compiler

function isMatch(s: string, p: string): boolean {
  let m: number = s.length;
  (globalThis as any).m = m;
  let n: number = p.length;
  (globalThis as any).n = n;
  let memo: Record<number, boolean> = {};
  (globalThis as any).memo = memo;
  function dfs(i: number, j: number): boolean {
    let key: number = (i * (n + 1)) + j;
    (globalThis as any).key = key;
    if (Object.prototype.hasOwnProperty.call(memo, String(key))) {
      return memo[key];
    }
    if ((j == n)) {
      return (i == m);
    }
    let ans: boolean = false;
    (globalThis as any).ans = ans;
    if ((_indexString(p, j) == "*")) {
      if (dfs(i, j + 1)) {
        ans = true;
      } else if (((i < m) && dfs(i + 1, j))) {
        ans = true;
      }
    } else {
      if (
        ((i < m) &&
          ((_indexString(p, j) == "?") ||
            (_indexString(p, j) == _indexString(s, i))))
      ) {
        if (dfs(i + 1, j + 1)) {
          ans = true;
        }
      }
    }
    memo[key] = ans;
    return ans;
  }
  return dfs(0, 0);
}

function test_example_1(): void {
  if (!(isMatch("aa", "a") == false)) throw new Error("expect failed");
}

function test_example_2(): void {
  if (!(isMatch("aa", "*") == true)) throw new Error("expect failed");
}

function test_example_3(): void {
  if (!(isMatch("cb", "?a") == false)) throw new Error("expect failed");
}

function test_example_4(): void {
  if (!(isMatch("adceb", "*a*b") == true)) throw new Error("expect failed");
}

function test_empty_pattern(): void {
  if (!(isMatch("", "") == true)) throw new Error("expect failed");
}

function test_only_star(): void {
  if (!(isMatch("abc", "*") == true)) throw new Error("expect failed");
}

function main(): void {
  test_example_1();
  test_example_2();
  test_example_3();
  test_example_4();
  test_empty_pattern();
  test_only_star();
}
function _indexString(s: string, i: number): string {
  const runes = Array.from(s);
  if (i < 0) i += runes.length;
  if (i < 0 || i >= runes.length) throw new Error("index out of range");
  return runes[i];
}

main();
