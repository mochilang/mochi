import * as fs from "fs";

const less20 = ["", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine",
  "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen"];
const tens = ["", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"];
const thousands = ["", "Thousand", "Million", "Billion"];

function helper(n: number): string {
  if (n === 0) return "";
  if (n < 20) return less20[n];
  if (n < 100) return tens[Math.floor(n / 10)] + (n % 10 === 0 ? "" : " " + helper(n % 10));
  return less20[Math.floor(n / 100)] + " Hundred" + (n % 100 === 0 ? "" : " " + helper(n % 100));
}

function solve(num: number): string {
  if (num === 0) return "Zero";
  const parts: string[] = [];
  let idx = 0;
  while (num > 0) {
    const chunk = num % 1000;
    if (chunk !== 0) {
      let words = helper(chunk);
      if (thousands[idx] !== "") words += " " + thousands[idx];
      parts.unshift(words);
    }
    num = Math.floor(num / 1000);
    idx++;
  }
  return parts.join(" ");
}

const lines = fs.readFileSync(0, "utf8").split(/\r?\n/);
if (lines.length > 0 && lines[0].trim() !== "") {
  const t = Number(lines[0].trim());
  const out: string[] = [];
  for (let i = 0; i < t; i++) out.push(solve(Number(lines[i + 1].trim())));
  process.stdout.write(out.join("\n"));
}
