import * as fs from "fs";

function calculate(expr: string): number {
  let result = 0;
  let number = 0;
  let sign = 1;
  const stack: number[] = [];
  for (const ch of expr) {
    if (ch >= "0" && ch <= "9") {
      number = number * 10 + Number(ch);
    } else if (ch === "+" || ch === "-") {
      result += sign * number;
      number = 0;
      sign = ch === "+" ? 1 : -1;
    } else if (ch === "(") {
      stack.push(result, sign);
      result = 0;
      number = 0;
      sign = 1;
    } else if (ch === ")") {
      result += sign * number;
      number = 0;
      const prevSign = stack.pop()!;
      const prevResult = stack.pop()!;
      result = prevResult + prevSign * result;
    }
  }
  return result + sign * number;
}

const lines = fs.readFileSync(0, "utf8").split(/\r?\n/);
if (lines.length > 0 && lines[0].trim() !== "") {
  const t = Number(lines[0].trim());
  const out: string[] = [];
  for (let i = 0; i < t; i++) {
    out.push(String(calculate(lines[i + 1] ?? "")));
  }
  process.stdout.write(out.join("\n"));
}
