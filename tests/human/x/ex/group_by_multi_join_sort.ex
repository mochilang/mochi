nation = [
  %{n_nationkey: 1, n_name: "BRAZIL"}
]

customer = [
  %{c_custkey: 1, c_name: "Alice", c_acctbal: 100.0, c_nationkey: 1,
    c_address: "123 St", c_phone: "123-456", c_comment: "Loyal"}
]

orders = [
  %{o_orderkey: 1000, o_custkey: 1, o_orderdate: "1993-10-15"},
  %{o_orderkey: 2000, o_custkey: 1, o_orderdate: "1994-01-02"}
]

lineitem = [
  %{l_orderkey: 1000, l_returnflag: "R", l_extendedprice: 1000.0, l_discount: 0.1},
  %{l_orderkey: 2000, l_returnflag: "N", l_extendedprice: 500.0, l_discount: 0.0}
]

start_date = "1993-10-01"
end_date = "1994-01-01"

result =
  for c <- customer,
      o <- orders,
      o.o_custkey == c.c_custkey,
      l <- lineitem,
      l.l_orderkey == o.o_orderkey,
      n <- nation,
      n.n_nationkey == c.c_nationkey,
      o.o_orderdate >= start_date,
      o.o_orderdate < end_date,
      l.l_returnflag == "R" do
    %{c: c, l: l}
  end
  |> Enum.group_by(fn %{c: c} -> {
      c.c_custkey, c.c_name, c.c_acctbal, c.c_address, c.c_phone, c.c_comment, hd(nation).n_name
    } end)
  |> Enum.map(fn {key, list} ->
    revenue = Enum.reduce(list, 0.0, fn %{l: l}, acc ->
      acc + l.l_extendedprice * (1 - l.l_discount)
    end)
    %{
      c_custkey: elem(key, 0),
      c_name: elem(key, 1),
      revenue: revenue,
      c_acctbal: elem(key, 2),
      n_name: elem(key, 6),
      c_address: elem(key, 3),
      c_phone: elem(key, 4),
      c_comment: elem(key, 5)
    }
  end)
  |> Enum.sort_by(& &1.revenue, :desc)

IO.inspect(result)
