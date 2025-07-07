using System;
using System.Collections.Generic;
using System.Linq;

class Nation
{
    public int n_nationkey;
    public string n_name;
}

class Customer
{
    public int c_custkey;
    public string c_name;
    public double c_acctbal;
    public int c_nationkey;
    public string c_address;
    public string c_phone;
    public string c_comment;
}

class Order
{
    public int o_orderkey;
    public int o_custkey;
    public string o_orderdate;
}

class LineItem
{
    public int l_orderkey;
    public string l_returnflag;
    public double l_extendedprice;
    public double l_discount;
}

class Program
{
    static void Main()
    {
        var nation = new List<Nation>
        {
            new Nation { n_nationkey = 1, n_name = "BRAZIL" },
        };
        var customer = new List<Customer>
        {
            new Customer
            {
                c_custkey = 1,
                c_name = "Alice",
                c_acctbal = 100.0,
                c_nationkey = 1,
                c_address = "123 St",
                c_phone = "123-456",
                c_comment = "Loyal",
            },
        };
        var orders = new List<Order>
        {
            new Order
            {
                o_orderkey = 1000,
                o_custkey = 1,
                o_orderdate = "1993-10-15",
            },
            new Order
            {
                o_orderkey = 2000,
                o_custkey = 1,
                o_orderdate = "1994-01-02",
            },
        };
        var lineitem = new List<LineItem>
        {
            new LineItem
            {
                l_orderkey = 1000,
                l_returnflag = "R",
                l_extendedprice = 1000.0,
                l_discount = 0.1,
            },
            new LineItem
            {
                l_orderkey = 2000,
                l_returnflag = "N",
                l_extendedprice = 500.0,
                l_discount = 0.0,
            },
        };

        string start_date = "1993-10-01";
        string end_date = "1994-01-01";

        var result =
            from c in customer
            join o in orders on c.c_custkey equals o.o_custkey
            join l in lineitem on o.o_orderkey equals l.l_orderkey
            join n in nation on c.c_nationkey equals n.n_nationkey
            where
                string.Compare(o.o_orderdate, start_date) >= 0
                && string.Compare(o.o_orderdate, end_date) < 0
                && l.l_returnflag == "R"
            group new
            {
                c,
                l,
                n,
            } by new
            {
                c.c_custkey,
                c.c_name,
                c.c_acctbal,
                c.c_address,
                c.c_phone,
                c.c_comment,
                n.n_name,
            } into g
            orderby -g.Sum(x => x.l.l_extendedprice * (1 - x.l.l_discount))
            select new
            {
                c_custkey = g.Key.c_custkey,
                c_name = g.Key.c_name,
                revenue = g.Sum(x => x.l.l_extendedprice * (1 - x.l.l_discount)),
                c_acctbal = g.Key.c_acctbal,
                n_name = g.Key.n_name,
                c_address = g.Key.c_address,
                c_phone = g.Key.c_phone,
                c_comment = g.Key.c_comment,
            };

        foreach (var r in result)
        {
            Console.WriteLine($"{r.c_name} revenue {r.revenue}");
        }
    }
}
