import * as fs from "fs";

function solve(s: string): string[] {
  let leftRemove = 0;
  let rightRemove = 0;
  for (const ch of s) {
    if (ch === "(") leftRemove++;
    else if (ch === ")") {
      if (leftRemove > 0) leftRemove--;
      else rightRemove++;
    }
  }

  const ans = new Set<string>();
  const path: string[] = [];

  function dfs(i: number, left: number, right: number, balance: number): void {
    if (i === s.length) {
      if (left === 0 && right === 0 && balance === 0) ans.add(path.join(""));
      return;
    }
    const ch = s[i];
    if (ch === "(") {
      if (left > 0) dfs(i + 1, left - 1, right, balance);
      path.push(ch);
      dfs(i + 1, left, right, balance + 1);
      path.pop();
    } else if (ch === ")") {
      if (right > 0) dfs(i + 1, left, right - 1, balance);
      if (balance > 0) {
        path.push(ch);
        dfs(i + 1, left, right, balance - 1);
        path.pop();
      }
    } else {
      path.push(ch);
      dfs(i + 1, left, right, balance);
      path.pop();
    }
  }

  dfs(0, leftRemove, rightRemove, 0);
  return [...ans].sort();
}

const lines = fs.readFileSync(0, "utf8").split(/\r?\n/);
if (lines.length > 0 && lines[0].trim() !== "") {
  const t = parseInt(lines[0].trim(), 10);
  const blocks: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const ans = solve(lines[tc + 1] ?? "");
    blocks.push([String(ans.length), ...ans].join("\n"));
  }
  process.stdout.write(blocks.join("\n\n"));
}
