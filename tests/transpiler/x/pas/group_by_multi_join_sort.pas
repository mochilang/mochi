{$mode objfpc}
program Main;
type Anon1 = record
  n_nationkey: integer;
  n_name: string;
end;
type Anon2 = record
  c_custkey: integer;
  c_name: string;
  c_acctbal: real;
  c_nationkey: integer;
  c_address: string;
  c_phone: string;
  c_comment: string;
end;
type Anon3 = record
  o_orderkey: integer;
  o_custkey: integer;
  o_orderdate: string;
end;
type Anon4 = record
  l_orderkey: integer;
  l_returnflag: string;
  l_extendedprice: real;
  l_discount: real;
end;
type Anon5 = record
  c_custkey: ;
  c_name: ;
  revenue: ;
  c_acctbal: ;
  n_name: ;
  c_address: ;
  c_phone: ;
  c_comment: ;
end;
function sumq0(arr0: integer): integer;
begin
  Result := 0;
  for x in arr0 do begin
  Result := Result + (x.l.l_extendedprice * (1 - x.l.l_discount));
end;
end;
function sumq1(arr1: integer): integer;
begin
  Result := 0;
  for x in arr1 do begin
  Result := Result + (x.l.l_extendedprice * (1 - x.l.l_discount));
end;
end;
var
  nation: array of Anon1;
  customer: array of Anon2;
  orders: array of Anon3;
  lineitem: array of Anon4;
  start_date: string;
  end_date: string;
  i6: integer;
  j7: integer;
  tmp8: Anon5;
  result: array of Anon5;
  c: Anon2;
  l: Anon4;
  o: Anon3;
  n: Anon1;
begin
  nation := [(n_nationkey: 1; n_name: 'BRAZIL')];
  customer := [(c_custkey: 1; c_name: 'Alice'; c_acctbal: 100; c_nationkey: 1; c_address: '123 St'; c_phone: '123-456'; c_comment: 'Loyal')];
  orders := [(o_orderkey: 1000; o_custkey: 1; o_orderdate: '1993-10-15'), (o_orderkey: 2000; o_custkey: 1; o_orderdate: '1994-01-02')];
  lineitem := [(l_orderkey: 1000; l_returnflag: 'R'; l_extendedprice: 1000; l_discount: 0.1), (l_orderkey: 2000; l_returnflag: 'N'; l_extendedprice: 500; l_discount: 0)];
  start_date := '1993-10-01';
  end_date := '1994-01-01';
  result := [];
  for c in customer do begin
  for o in orders do begin
  for l in lineitem do begin
  for n in nation do begin
  if (((o.o_custkey = c.c_custkey) and (l.l_orderkey = o.o_orderkey)) and (n.n_nationkey = c.c_nationkey)) and (((o.o_orderdate >= start_date) and (o.o_orderdate < end_date)) and (l.l_returnflag = 'R')) then begin
  result := concat(result, [(c_custkey: g.key.c_custkey; c_name: g.key.c_name; revenue: sumq0(g); c_acctbal: g.key.c_acctbal; n_name: g.key.n_name; c_address: g.key.c_address; c_phone: g.key.c_phone; c_comment: g.key.c_comment)]);
end;
end;
end;
end;
end;
  for i6 := 0 to (Length(result) - 1 - 1) do begin
  for j7 := i6 + 1 to (Length(result) - 1) do begin
  if sumq1(g) < sumq1(g) then begin
  tmp8 := result[i6];
  result[i6] := result[j7];
  result[j7] := tmp8;
end;
end;
end;
  writeln(result);
end.
