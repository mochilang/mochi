struct Nation { let n_nationkey: Int; let n_name: String }
struct Customer {
    let c_custkey: Int; let c_name: String; let c_acctbal: Double
    let c_nationkey: Int; let c_address: String; let c_phone: String; let c_comment: String
}
struct Order { let o_orderkey: Int; let o_custkey: Int; let o_orderdate: String }
struct LineItem { let l_orderkey: Int; let l_returnflag: String; let l_extendedprice: Double; let l_discount: Double }

let nation = [Nation(n_nationkey: 1, n_name: "BRAZIL")]
let customer = [Customer(c_custkey: 1, c_name: "Alice", c_acctbal: 100.0, c_nationkey: 1, c_address: "123 St", c_phone: "123-456", c_comment: "Loyal")]
let orders = [
    Order(o_orderkey: 1000, o_custkey: 1, o_orderdate: "1993-10-15"),
    Order(o_orderkey: 2000, o_custkey: 1, o_orderdate: "1994-01-02")
]
let lineitem = [
    LineItem(l_orderkey: 1000, l_returnflag: "R", l_extendedprice: 1000.0, l_discount: 0.1),
    LineItem(l_orderkey: 2000, l_returnflag: "N", l_extendedprice: 500.0, l_discount: 0.0)
]

let start_date = "1993-10-01"
let end_date = "1994-01-01"

struct Result {
    let c_custkey: Int
    let c_name: String
    let revenue: Double
    let c_acctbal: Double
    let n_name: String
    let c_address: String
    let c_phone: String
    let c_comment: String
}

var totals: [Int: Result] = [:]
for c in customer {
    for o in orders where o.o_custkey == c.c_custkey {
        if o.o_orderdate >= start_date && o.o_orderdate < end_date {
            for l in lineitem where l.l_orderkey == o.o_orderkey && l.l_returnflag == "R" {
                if let n = nation.first(where: { $0.n_nationkey == c.c_nationkey }) {
                    let rev = l.l_extendedprice * (1 - l.l_discount)
                    if var existing = totals[c.c_custkey] {
                        existing = Result(c_custkey: existing.c_custkey,
                                         c_name: existing.c_name,
                                         revenue: existing.revenue + rev,
                                         c_acctbal: existing.c_acctbal,
                                         n_name: existing.n_name,
                                         c_address: existing.c_address,
                                         c_phone: existing.c_phone,
                                         c_comment: existing.c_comment)
                        totals[c.c_custkey] = existing
                    } else {
                        totals[c.c_custkey] = Result(c_custkey: c.c_custkey,
                                                     c_name: c.c_name,
                                                     revenue: rev,
                                                     c_acctbal: c.c_acctbal,
                                                     n_name: n.n_name,
                                                     c_address: c.c_address,
                                                     c_phone: c.c_phone,
                                                     c_comment: c.c_comment)
                    }
                }
            }
        }
    }
}

var result: [Result] = Array(totals.values)
result.sort { $0.revenue > $1.revenue }

print(result)
