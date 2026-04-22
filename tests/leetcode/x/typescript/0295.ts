import * as fs from "fs";

class MedianFinder {
    private data: number[] = [];

    addNum(num: number): void {
        let lo = 0;
        let hi = this.data.length;
        while (lo < hi) {
            const mid = (lo + hi) >> 1;
            if (this.data[mid] < num) lo = mid + 1;
            else hi = mid;
        }
        this.data.splice(lo, 0, num);
    }

    findMedian(): number {
        const n = this.data.length;
        if (n % 2 === 1) return this.data[n >> 1];
        return (this.data[n / 2 - 1] + this.data[n / 2]) / 2;
    }
}

const lines = fs.readFileSync(0, "utf8").split(/\r?\n/).map(s => s.trim()).filter(Boolean);
if (lines.length > 0) {
    const t = Number(lines[0]);
    let idx = 1;
    const blocks: string[] = [];
    for (let tc = 0; tc < t; tc++) {
        const m = Number(lines[idx++]);
        const mf = new MedianFinder();
        const out: string[] = [];
        for (let i = 0; i < m; i++) {
            const parts = lines[idx++].split(/\s+/);
            if (parts[0] === "addNum") {
                mf.addNum(Number(parts[1]));
            } else {
                out.push(mf.findMedian().toFixed(1));
            }
        }
        blocks.push(out.join("\n"));
    }
    process.stdout.write(blocks.join("\n\n"));
}
