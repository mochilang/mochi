import * as fs from "fs";

function minArea(image: string[], x: number, y: number): number {
  let top = image.length;
  let bottom = -1;
  let left = image[0].length;
  let right = -1;
  for (let i = 0; i < image.length; i++) {
    for (let j = 0; j < image[i].length; j++) {
      if (image[i][j] === "1") {
        top = Math.min(top, i);
        bottom = Math.max(bottom, i);
        left = Math.min(left, j);
        right = Math.max(right, j);
      }
    }
  }
  return (bottom - top + 1) * (right - left + 1);
}

const lines = fs.readFileSync(0, "utf8").split(/\r?\n/).map(s => s.trim()).filter(Boolean);
if (lines.length > 0) {
  const t = parseInt(lines[0], 10);
  let idx = 1;
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const [rStr] = lines[idx++].split(/\s+/);
    const r = parseInt(rStr, 10);
    const image = lines.slice(idx, idx + r);
    idx += r;
    const [xStr, yStr] = lines[idx++].split(/\s+/);
    out.push(String(minArea(image, parseInt(xStr, 10), parseInt(yStr, 10))));
  }
  process.stdout.write(out.join("\n\n"));
}
