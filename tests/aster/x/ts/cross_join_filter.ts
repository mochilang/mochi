// Generated by Mochi v0.10.35 on 2025-07-22 14:07:01 GMT+7
export interface Result { n: number; l: string }
const nums: number[] = [1, 2, 3];
const letters: string[] = ["A", "B"];
const pairs: Result[] = (() => {
    let _items = nums.map(v => [v]);
    {
        const _next = [];
        for (const it of _items) {
            for (const l of letters) {
                _next.push([...it, l]);
            }
        }
        _items = _next;
    }
    let _rows = _items;
    _rows = _rows.filter(r => {
        const [n, l] = r;
        return ((n % 2) == 0);
    });
    const result: Result[] = [];
    for (const r of _rows) {
        const [n, l] = r;
        result.push({n, l});
    }
    return result;
})();
console.log(String("--- Even pairs ---"));
for (const p of pairs) {
    console.log((String(p["n"]) + " " + String(p["l"])).trim());
}
