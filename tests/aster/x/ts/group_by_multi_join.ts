// Generated by Mochi v0.10.35 on 2025-07-22 14:07:04 GMT+7
export interface Result { id: number; name: string }
export interface Nation { id: number; name: string }
export interface Supplier { id: number; nation: number }
export interface Partsupp { part: number; supplier: number; cost: number; qty: number }
const nations: Nation[] = [{id: 1, name: "A"}, {id: 2, name: "B"}];
const suppliers: Supplier[] = [{id: 1, nation: 1}, {id: 2, nation: 2}];
const partsupp: Partsupp[] = [{part: 100, supplier: 1, cost: 10.0, qty: 2}, {part: 100, supplier: 2, cost: 20.0, qty: 1}, {part: 200, supplier: 1, cost: 5.0, qty: 3}];
const filtered: Result[] = (() => {
    const result: Result[] = [];
    for (const ps of partsupp) {
        for (const s of suppliers) {
            if ((!((s["id"] == ps["supplier"]))))
                continue
            for (const n of nations) {
                if ((!((n["id"] == s["nation"]))))
                    continue
                if ((!((n["name"] == "A"))))
                    continue
                result.push({part: ps["part"], value: (ps["cost"] * ps["qty"])});
            }
        }
    }
    return result;
})();
const grouped: Record<string, any>[] = (() => {
    const groups = new Map<string, { key: any; items: any[] }>;
    for (const x of filtered) {
        const k = x["part"];
        const ks = JSON.stringify(k);
        let g = groups.get(ks);
        if ((!g)) {
            g = {key: k, items: []};
            groups.set(ks, g);
        }
        g.items.push(x);
    }
    let ordered = Array.from(groups.values());
    const result: Result[] = [];
    for (const g of ordered) {
        result.push({part: g["key"], total: (() => {
            const result = [];
            for (const r of g["items"]) {
                result.push(r["value"]);
            }
            const out = result;
            return out;
        })().reduce((a, b) => a + b, 0.0)});
    }
    return result;
})();
console.log(String("[" + (grouped).join(", ") + "]"));
