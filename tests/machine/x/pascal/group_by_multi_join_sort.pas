program GroupByMultiJoinSort;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

  generic procedure _printList<T>(arr: specialize TArray<T>);

var i: Integer;
begin
  for i := 0 to High(arr) do
    begin
      if i > 0 then Write(' ');
      Write(arr[i]);
    end;
  writeln();
end;

generic procedure _sortBy<T>(var arr: specialize TArray<T>; keys: specialize TArray<Variant>);

var i,j: integer;
  tmp: T;
  k: Variant;
begin
  for i := 0 to High(arr) - 1 do
    for j := i + 1 to High(arr) do
      if keys[i] > keys[j] then
        begin
          tmp := arr[i];
          arr[i] := arr[j];
          arr[j] := tmp;
          k := keys[i];
          keys[i] := keys[j];
          keys[j] := k;
        end;
end;

generic function _sumList<T>(arr: specialize TArray<T>): double;

var i: integer;
  s: double;
begin
  s := 0;
  for i := 0 to High(arr) do
    s := s + arr[i];
  Result := s;
end;

var
  _tmp0: specialize TFPGMap<string, Variant>;
  _tmp1: specialize TFPGMap<string, Variant>;
  _tmp10: specialize TArray<Variant>;
  _tmp2: specialize TFPGMap<string, Variant>;
  _tmp3: specialize TFPGMap<string, Variant>;
  _tmp4: specialize TFPGMap<string, Variant>;
  _tmp5: specialize TFPGMap<string, Variant>;
  _tmp6: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp7: specialize TArray<Variant>;
  _tmp8: specialize TFPGMap<string, Variant>;
  _tmp9: specialize TArray<Variant>;
  c: specialize TFPGMap<string, Variant>;
  customer: specialize TArray<specialize TFPGMap<string, Variant>>;
  end_date: string;
  lineitem: specialize TArray<specialize TFPGMap<string, Variant>>;
  nation: specialize TArray<specialize TFPGMap<string, Variant>>;
  orders: specialize TArray<specialize TFPGMap<string, Variant>>;
  _result: specialize TArray<specialize TFPGMap<string, Variant>>;
  start_date: string;
  x: integer;

begin
  _tmp0 := specialize TFPGMap<string, Variant>.Create;
  _tmp0.AddOrSetData('n_nationkey', 1);
  _tmp0.AddOrSetData('n_name', 'BRAZIL');
  nation := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp0]);
  _tmp1 := specialize TFPGMap<string, Variant>.Create;
  _tmp1.AddOrSetData('c_custkey', 1);
  _tmp1.AddOrSetData('c_name', 'Alice');
  _tmp1.AddOrSetData('c_acctbal', 100);
  _tmp1.AddOrSetData('c_nationkey', 1);
  _tmp1.AddOrSetData('c_address', '123 St');
  _tmp1.AddOrSetData('c_phone', '123-456');
  _tmp1.AddOrSetData('c_comment', 'Loyal');
  customer := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp1]);
  _tmp2 := specialize TFPGMap<string, Variant>.Create;
  _tmp2.AddOrSetData('o_orderkey', 1000);
  _tmp2.AddOrSetData('o_custkey', 1);
  _tmp2.AddOrSetData('o_orderdate', '1993-10-15');
  _tmp3 := specialize TFPGMap<string, Variant>.Create;
  _tmp3.AddOrSetData('o_orderkey', 2000);
  _tmp3.AddOrSetData('o_custkey', 1);
  _tmp3.AddOrSetData('o_orderdate', '1994-01-02');
  orders := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp2, _tmp3]);
  _tmp4 := specialize TFPGMap<string, Variant>.Create;
  _tmp4.AddOrSetData('l_orderkey', 1000);
  _tmp4.AddOrSetData('l_returnflag', 'R');
  _tmp4.AddOrSetData('l_extendedprice', 1000);
  _tmp4.AddOrSetData('l_discount', 0.1);
  _tmp5 := specialize TFPGMap<string, Variant>.Create;
  _tmp5.AddOrSetData('l_orderkey', 2000);
  _tmp5.AddOrSetData('l_returnflag', 'N');
  _tmp5.AddOrSetData('l_extendedprice', 500);
  _tmp5.AddOrSetData('l_discount', 0);
  lineitem := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp4, _tmp5]);
  start_date := '1993-10-01';
  end_date := '1994-01-01';
  SetLength(_tmp6, 0);
  SetLength(_tmp7, 0);
  for c in customer do
    begin
      for o in orders do
        begin
          if not ((o.o_custkey = c.KeyData['c_custkey'])) then continue;
          for l in lineitem do
            begin
              if not ((l.l_orderkey = o.o_orderkey)) then continue;
              for n in nation do
                begin
                  if not ((n.n_nationkey = c.KeyData['c_nationkey'])) then continue;
                  if not ((((o.KeyData['o_orderdate'] >= start_date) and (o.KeyData['o_orderdate'] <
                     end_date)) and (l.KeyData['l_returnflag'] = 'R'))) then continue;
                  _tmp8 := specialize TFPGMap<string, Variant>.Create;
                  _tmp8.AddOrSetData('c_custkey', g.key.c_custkey);
                  _tmp8.AddOrSetData('c_name', g.key.c_name);
                  SetLength(_tmp9, 0);
                  for x in g do
                    begin
                      _tmp9 := Concat(_tmp9, [x.l.l_extendedprice * 1 - x.l.l_discount]);
                    end;
                  _tmp8.AddOrSetData('revenue', specialize _sumList<Variant>(_tmp9));
                  _tmp8.AddOrSetData('c_acctbal', g.key.c_acctbal);
                  _tmp8.AddOrSetData('n_name', g.key.n_name);
                  _tmp8.AddOrSetData('c_address', g.key.c_address);
                  _tmp8.AddOrSetData('c_phone', g.key.c_phone);
                  _tmp8.AddOrSetData('c_comment', g.key.c_comment);
                  _tmp6 := Concat(_tmp6, [_tmp8]);
                  SetLength(_tmp10, 0);
                  for x in g do
                    begin
                      _tmp10 := Concat(_tmp10, [x.l.l_extendedprice * 1 - x.l.l_discount]);
                    end;
                  _tmp7 := Concat(_tmp7, [-specialize _sumList<Variant>(_tmp10)]);
                end;
            end;
        end;
    end;
  specialize _sortBy<specialize TFPGMap<string, Variant>>(_tmp6, _tmp7);
  _result := _tmp6;
  specialize _printList<specialize TFPGMap<string, Variant>>(_result);
end.
