// Generated by Mochi TypeScript compiler

function lengthOfLongestSubstring(s: string): number {
  let n: number = s.length;
  (globalThis as any).n = n;
  let start: number = 0;
  (globalThis as any).start = start;
  let best: number = 0;
  (globalThis as any).best = best;
  let i: number = 0;
  (globalThis as any).i = i;
  while ((i < n)) {
    let j: number = start;
    (globalThis as any).j = j;
    while ((j < i)) {
      if ((_indexString(s, j) == _indexString(s, i))) {
        start = j + 1;
        break;
      }
      j = j + 1;
    }
    let length: number = (i - start) + 1;
    (globalThis as any).length = length;
    if ((length > best)) {
      best = length;
    }
    i = i + 1;
  }
  return best;
}

function test_example_1(): void {
  if (!(lengthOfLongestSubstring("abcabcbb") == 3)) {
    throw new Error("expect failed");
  }
}

function test_example_2(): void {
  if (!(lengthOfLongestSubstring("bbbbb") == 1)) {
    throw new Error("expect failed");
  }
}

function test_example_3(): void {
  if (!(lengthOfLongestSubstring("pwwkew") == 3)) {
    throw new Error("expect failed");
  }
}

function test_empty_string(): void {
  if (!(lengthOfLongestSubstring("") == 0)) throw new Error("expect failed");
}

function main(): void {
  test_example_1();
  test_example_2();
  test_example_3();
  test_empty_string();
}
function _indexString(s: string, i: number): string {
  const runes = Array.from(s);
  if (i < 0) i += runes.length;
  if (i < 0 || i >= runes.length) throw new Error("index out of range");
  return runes[i];
}

main();
