// Generated by Mochi v0.10.35 on 2025-07-22 14:07:04 GMT+7
export interface Result { n_nationkey: number; n_name: string }
export interface Nation { n_nationkey: number; n_name: string }
export interface Customer { c_custkey: number; c_name: string; c_acctbal: number; c_nationkey: number; c_address: string; c_phone: string; c_comment: string }
export interface Order { o_orderkey: number; o_custkey: number; o_orderdate: string }
export interface Lineitem { l_orderkey: number; l_returnflag: string; l_extendedprice: number; l_discount: number }
const nation: Nation[] = [{n_nationkey: 1, n_name: "BRAZIL"}];
const customer: Customer[] = [{c_custkey: 1, c_name: "Alice", c_acctbal: 100.0, c_nationkey: 1, c_address: "123 St", c_phone: "123-456", c_comment: "Loyal"}];
const orders: Order[] = [{o_orderkey: 1000, o_custkey: 1, o_orderdate: "1993-10-15"}, {o_orderkey: 2000, o_custkey: 1, o_orderdate: "1994-01-02"}];
const lineitem: Lineitem[] = [{l_orderkey: 1000, l_returnflag: "R", l_extendedprice: 1000.0, l_discount: 0.1}, {l_orderkey: 2000, l_returnflag: "N", l_extendedprice: 500.0, l_discount: 0.0}];
const start_date: string = "1993-10-01";
const end_date: string = "1994-01-01";
const result: Record<string, any>[] = (() => {
    const groups = new Map<string, { key: any; items: any[] }>;
    let rows = customer.map(v => [v]);
    {
        const joined = [];
        const arr = orders;
        for (const left of rows) {
            const [c] = left;
            let m = false;
            for (let ri = 0; ri < arr.length; ++ri) {
                const o = arr[ri];
                if ((!((o["o_custkey"] == c["c_custkey"]))))
                    continue
                m = true;
                joined.push([...left, o]);
            }
        }
        rows = joined;
    }
    {
        const joined = [];
        const arr = lineitem;
        for (const left of rows) {
            const [c, o] = left;
            let m = false;
            for (let ri = 0; ri < arr.length; ++ri) {
                const l = arr[ri];
                if ((!((l["l_orderkey"] == o["o_orderkey"]))))
                    continue
                m = true;
                joined.push([...left, l]);
            }
        }
        rows = joined;
    }
    {
        const joined = [];
        const arr = nation;
        for (const left of rows) {
            const [c, o, l] = left;
            let m = false;
            for (let ri = 0; ri < arr.length; ++ri) {
                const n = arr[ri];
                if ((!((n["n_nationkey"] == c["c_nationkey"]))))
                    continue
                m = true;
                joined.push([...left, n]);
            }
        }
        rows = joined;
    }
    for (const it of rows) {
        const [c, o, l, n] = it;
        if ((!((((o["o_orderdate"] >= start_date) && (o["o_orderdate"] < end_date)) && (l["l_returnflag"] == "R")))))
            continue
        const k = {c_custkey: c["c_custkey"], c_name: c["c_name"], c_acctbal: c["c_acctbal"], c_address: c["c_address"], c_phone: c["c_phone"], c_comment: c["c_comment"], n_name: n["n_name"]};
        const ks = JSON.stringify(k);
        let g = groups.get(ks);
        if ((!g)) {
            g = {key: k, items: []};
            groups.set(ks, g);
        }
        g.items.push({c, o, l, n});
    }
    let ordered = Array.from(groups.values());
    const result: Result[] = [];
    const pairs = ordered.map(grp => {
        const g = grp;
        return {g: g, key: -(() => {
            const result = [];
            for (const x of g["items"]) {
                result.push((x["l"]["l_extendedprice"] * (1 - x["l"]["l_discount"])));
            }
            const out = result;
            return out;
        })().reduce((a, b) => a + b, 0.0)};
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
        result.push({c_custkey: g["key"]["c_custkey"], c_name: g["key"]["c_name"], revenue: (() => {
            const result = [];
            for (const x of g["items"]) {
                result.push((x["l"]["l_extendedprice"] * (1 - x["l"]["l_discount"])));
            }
            const out = result;
            return out;
        })().reduce((a, b) => a + b, 0.0), c_acctbal: g["key"]["c_acctbal"], n_name: g["key"]["n_name"], c_address: g["key"]["c_address"], c_phone: g["key"]["c_phone"], c_comment: g["key"]["c_comment"]});
    }
    return result;
})();
console.log(String("[" + (result).join(", ") + "]"));
