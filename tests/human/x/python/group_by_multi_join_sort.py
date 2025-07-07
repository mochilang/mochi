nation = [
    {"n_nationkey": 1, "n_name": "BRAZIL"},
]
customer = [
    {
        "c_custkey": 1,
        "c_name": "Alice",
        "c_acctbal": 100.0,
        "c_nationkey": 1,
        "c_address": "123 St",
        "c_phone": "123-456",
        "c_comment": "Loyal",
    }
]
orders = [
    {"o_orderkey": 1000, "o_custkey": 1, "o_orderdate": "1993-10-15"},
    {"o_orderkey": 2000, "o_custkey": 1, "o_orderdate": "1994-01-02"},
]
lineitem = [
    {"l_orderkey": 1000, "l_returnflag": "R", "l_extendedprice": 1000.0, "l_discount": 0.1},
    {"l_orderkey": 2000, "l_returnflag": "N", "l_extendedprice": 500.0, "l_discount": 0.0},
]

start_date = "1993-10-01"
end_date = "1994-01-01"

rows = []
for c in customer:
    for o in [o for o in orders if o["o_custkey"] == c["c_custkey"]]:
        for l in [l for l in lineitem if l["l_orderkey"] == o["o_orderkey"]]:
            for n in [n for n in nation if n["n_nationkey"] == c["c_nationkey"]]:
                if o["o_orderdate"] >= start_date and o["o_orderdate"] < end_date and l["l_returnflag"] == "R":
                    rows.append({"c": c, "o": o, "l": l, "n": n})

groups = {}
for r in rows:
    key = (
        r["c"]["c_custkey"],
        r["c"]["c_name"],
        r["c"]["c_acctbal"],
        r["c"]["c_address"],
        r["c"]["c_phone"],
        r["c"]["c_comment"],
        r["n"]["n_name"],
    )
    groups.setdefault(key, []).append(r)

result = []
for key, grp in groups.items():
    revenue = sum(x["l"]["l_extendedprice"] * (1 - x["l"]["l_discount"]) for x in grp)
    result.append({
        "c_custkey": key[0],
        "c_name": key[1],
        "revenue": revenue,
        "c_acctbal": key[2],
        "n_name": key[6],
        "c_address": key[3],
        "c_phone": key[4],
        "c_comment": key[5],
    })

result.sort(key=lambda r: -r["revenue"])
print(result)
