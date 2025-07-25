// Generated by Mochi v0.10.41 on 2025-07-26 17:30:32 GMT+7

function collapse(s: string): any[] {
  let i: number = 0;
  let prev: string = "";
  let res: string = "";
  let orig: number = (Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length);
  while ((i < (Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length))) {
    let ch: string = (s).substring(i, (i + 1));
    if ((ch != prev)) {
      res = (res + ch);
      prev = ch;
    }
    i = (i + 1);
  }
  return [res, orig, (Array.isArray(res) || typeof res === 'string' ? res.length : Object.keys(res ?? {}).length)];
}
function main() {
  let strings: string[] = ["", "\"If I were two-faced, would I be wearing this one?\" --- Abraham Lincoln ", "..111111111111111111111111111111111111111111111111111111111111111777888", "I never give 'em hell, I just tell the truth, and they think it's hell. ", "                                                   ---  Harry S Truman ", "The better the 4-wheel drive, the further you'll be from help when ya get stuck!", "headmistressship", "aardvark", "😍😀🙌💃😍😍😍🙌"];
  let idx: number = 0;
  while ((idx < (Array.isArray(strings) || typeof strings === 'string' ? strings.length : Object.keys(strings ?? {}).length))) {
    let s: string = strings[idx];
    let r: any[] = collapse(s);
    let cs = r[Math.trunc(0)];
    let olen = r[Math.trunc(1)];
    let clen = r[Math.trunc(2)];
    console.log((((("original : length = " + String(olen)) + ", string = «««") + s) + "»»»"));
    console.log((((("collapsed: length = " + String(clen)) + ", string = «««") + cs) + "»»»\n"));
    idx = (idx + 1);
  }
}
main();
