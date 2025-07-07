struct Nation { let id: Int; let name: String }
struct Supplier { let id: Int; let nation: Int }
struct PartSupp { let part: Int; let supplier: Int; let cost: Double; let qty: Int }

let nations = [
    Nation(id: 1, name: "A"),
    Nation(id: 2, name: "B")
]
let suppliers = [
    Supplier(id: 1, nation: 1),
    Supplier(id: 2, nation: 2)
]
let partsupp = [
    PartSupp(part: 100, supplier: 1, cost: 10.0, qty: 2),
    PartSupp(part: 100, supplier: 2, cost: 20.0, qty: 1),
    PartSupp(part: 200, supplier: 1, cost: 5.0, qty: 3)
]

var filtered: [(part: Int, value: Double)] = []
for ps in partsupp {
    if let s = suppliers.first(where: { $0.id == ps.supplier }),
       let n = nations.first(where: { $0.id == s.nation }),
       n.name == "A" {
        filtered.append((part: ps.part, value: ps.cost * Double(ps.qty)))
    }
}

var totals: [Int: Double] = [:]
for f in filtered {
    totals[f.part, default: 0] += f.value
}

var grouped: [(part: Int, total: Double)] = []
for (part, total) in totals {
    grouped.append((part: part, total: total))
}

print(grouped)
