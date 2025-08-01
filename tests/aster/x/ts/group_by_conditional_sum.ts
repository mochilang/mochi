// Generated by Mochi v0.10.35 on 2025-07-22 14:07:03 GMT+7
export interface Result { cat: string; val: number; flag: boolean }
export interface Item { cat: string; val: number; flag: boolean }
const items: Item[] = [{cat: "a", val: 10, flag: true}, {cat: "a", val: 5, flag: false}, {cat: "b", val: 20, flag: true}];
const result: Record<string, any>[] = (() => {
    const groups = new Map<string, { key: any; items: any[] }>;
    for (const i of items) {
        const k = i["cat"];
        const ks = JSON.stringify(k);
        let g = groups.get(ks);
        if ((!g)) {
            g = {key: k, items: []};
            groups.set(ks, g);
        }
        g.items.push(i);
    }
    let ordered = Array.from(groups.values());
    const result: Result[] = [];
    const pairs = ordered.map(grp => {
        const g = grp;
        return {g: g, key: g["key"]};
    });
    pairs.sort((a, b) => {
        const ak = a.key;
        const bk = b.key;
        if ((ak < bk))
            return -1;
        if ((ak > bk))
            return 1;
        const sak = JSON.stringify(ak);
        const sbk = JSON.stringify(bk);
        return sak < sbk ? -1 : sak > sbk ? 1 : 0;
    });
    ordered = pairs.map(p => p.g);
    for (const g of ordered) {
        result.push({cat: g["key"], share: Math.trunc((() => {
            const result = [];
            for (const x of g["items"]) {
                result.push((x["flag"] ? x["val"] : 0));
            }
            const out = result;
            return out;
        })().reduce((a, b) => a + b, 0.0) / (() => {
            const result = [];
            for (const x of g["items"]) {
                result.push(x["val"]);
            }
            const out = result;
            return out;
        })().reduce((a, b) => a + b, 0.0))});
    }
    return result;
})();
console.log(String("[" + (result).join(", ") + "]"));
