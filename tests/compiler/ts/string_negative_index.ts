// Generated by Mochi TypeScript compiler

let text: string;

function main(): void {
  text = "hello";
  console.log(_indexString(text, -1));
}
function _indexString(s: string, i: number): string {
  const runes = Array.from(s);
  if (i < 0) i += runes.length;
  if (i < 0 || i >= runes.length) throw new Error("index out of range");
  return runes[i];
}

main();
