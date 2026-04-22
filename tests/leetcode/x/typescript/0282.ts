import * as fs from "fs";

function solve(num: string, target: number): string[] {
  const ans: string[] = [];
  function dfs(i: number, expr: string, value: number, last: number) {
    if (i === num.length) {
      if (value === target) ans.push(expr);
      return;
    }
    for (let j = i; j < num.length; j++) {
      if (j > i && num[i] === "0") break;
      const s = num.slice(i, j + 1);
      const n = Number(s);
      if (i === 0) {
        dfs(j + 1, s, n, n);
      } else {
        dfs(j + 1, expr + "+" + s, value + n, n);
        dfs(j + 1, expr + "-" + s, value - n, -n);
        dfs(j + 1, expr + "*" + s, value - last + last * n, last * n);
      }
    }
  }
  dfs(0, "", 0, 0);
  ans.sort();
  return ans;
}

const lines = fs.readFileSync(0, "utf8").split(/\r?\n/);
if (lines.length > 0 && lines[0].trim() !== "") {
  const t = Number(lines[0].trim());
  const blocks: string[] = [];
  let idx = 1;
  for (let tc = 0; tc < t; tc++) {
    const num = (lines[idx++] ?? "").trim();
    const target = Number((lines[idx++] ?? "").trim());
    const ans = solve(num, target);
    blocks.push([String(ans.length), ...ans].join("\n"));
  }
  process.stdout.write(blocks.join("\n\n"));
}
