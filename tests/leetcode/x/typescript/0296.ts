import * as fs from "fs";

function minTotalDistance(grid: number[][]): number {
    const rows: number[] = [];
    const cols: number[] = [];
    for (let i = 0; i < grid.length; i++) {
        for (let j = 0; j < grid[i].length; j++) {
            if (grid[i][j] === 1) rows.push(i);
        }
    }
    for (let j = 0; j < grid[0].length; j++) {
        for (let i = 0; i < grid.length; i++) {
            if (grid[i][j] === 1) cols.push(j);
        }
    }
    const mr = rows[Math.floor(rows.length / 2)];
    const mc = cols[Math.floor(cols.length / 2)];
    return rows.reduce((s, r) => s + Math.abs(r - mr), 0) + cols.reduce((s, c) => s + Math.abs(c - mc), 0);
}

const data = fs.readFileSync(0, "utf8").trim().split(/\s+/).filter(Boolean).map(Number);
if (data.length > 0) {
    let idx = 0;
    const t = data[idx++];
    const out: string[] = [];
    for (let tc = 0; tc < t; tc++) {
        const r = data[idx++];
        const c = data[idx++];
        const grid: number[][] = [];
        for (let i = 0; i < r; i++) {
            grid.push(data.slice(idx, idx + c));
            idx += c;
        }
        out.push(String(minTotalDistance(grid)));
    }
    process.stdout.write(out.join("\n\n"));
}
