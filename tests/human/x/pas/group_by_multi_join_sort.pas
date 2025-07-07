program GroupByMultiJoinSort;

type
  TNation = record
    n_nationkey: integer;
    n_name: string;
  end;

  TCustomer = record
    c_custkey: integer;
    c_name: string;
    c_acctbal: real;
    c_nationkey: integer;
    c_address: string;
    c_phone: string;
    c_comment: string;
  end;

  TOrder = record
    o_orderkey: integer;
    o_custkey: integer;
    o_orderdate: string;
  end;

  TLine = record
    l_orderkey: integer;
    l_returnflag: string;
    l_extendedprice: real;
    l_discount: real;
  end;

  TRow = record
    key: integer;
    revenue: real;
  end;

var
  nation: array[1..1] of TNation = (
    (n_nationkey:1; n_name:'BRAZIL')
  );
  customer: array[1..1] of TCustomer = (
    (c_custkey:1; c_name:'Alice'; c_acctbal:100.0; c_nationkey:1;
     c_address:'123 St'; c_phone:'123-456'; c_comment:'Loyal')
  );
  orders: array[1..2] of TOrder = (
    (o_orderkey:1000; o_custkey:1; o_orderdate:'1993-10-15'),
    (o_orderkey:2000; o_custkey:1; o_orderdate:'1994-01-02')
  );
  lineitem: array[1..2] of TLine = (
    (l_orderkey:1000; l_returnflag:'R'; l_extendedprice:1000.0; l_discount:0.1),
    (l_orderkey:2000; l_returnflag:'N'; l_extendedprice:500.0; l_discount:0.0)
  );
  rows: array[1..2] of TRow;
  rowCount: integer = 0;
  i,j,k,l: integer;
  rev: real;
  tmp: TRow;
begin
  for i := 1 to Length(customer) do
    for j := 1 to Length(orders) do
      if orders[j].o_custkey = customer[i].c_custkey then
        for k := 1 to Length(lineitem) do
          if lineitem[k].l_orderkey = orders[j].o_orderkey then
            for l := 1 to Length(nation) do
              if nation[l].n_nationkey = customer[i].c_nationkey then
                if (orders[j].o_orderdate >= '1993-10-01') and
                   (orders[j].o_orderdate < '1994-01-01') and
                   (lineitem[k].l_returnflag = 'R') then
                begin
                  rev := lineitem[k].l_extendedprice * (1 - lineitem[k].l_discount);
                  Inc(rowCount);
                  rows[rowCount].key := customer[i].c_custkey;
                  rows[rowCount].revenue := rev;
                end;

  { simple bubble sort by revenue descending }
  for i := 1 to rowCount-1 do
    for j := i+1 to rowCount do
      if rows[j].revenue > rows[i].revenue then
      begin
        tmp := rows[i];
        rows[i] := rows[j];
        rows[j] := tmp;
      end;

  for i := 1 to rowCount do
    Writeln('custkey=', rows[i].key, ' revenue=', rows[i].revenue:0:1);
end.
