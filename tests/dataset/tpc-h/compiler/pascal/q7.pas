// Generated by Mochi compiler v0.10.25 on 2025-07-13T14:11:43Z
program Q7;
{$mode objfpc}
{$modeswitch nestedprocvars}
uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

generic procedure _json<T>(v: T);
begin
  writeln('[]');
end;

generic procedure _sortBy<T>(var arr: specialize TArray<T>; keys: specialize TArray<Variant>);
var i,j: integer; tmp: T; k: Variant;
begin
  for i := 0 to High(arr) - 1 do
  for j := i + 1 to High(arr) do
    if keys[i] > keys[j] then
    begin
      tmp := arr[i]; arr[i] := arr[j]; arr[j] := tmp;
      k := keys[i]; keys[i] := keys[j]; keys[j] := k;
    end;
end;

generic function _sumList<T>(arr: specialize TArray<T>): double;
var i: integer; s: double;
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
  _tmp2: specialize TFPGMap<string, integer>;
  _tmp3: specialize TFPGMap<string, integer>;
  _tmp4: specialize TFPGMap<string, integer>;
  _tmp5: specialize TFPGMap<string, Variant>;
  _tmp6: specialize TFPGMap<string, Variant>;
  _tmp7: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp8: specialize TArray<Variant>;
  _tmp9: specialize TFPGMap<string, Variant>;
  c: specialize TFPGMap<string, integer>;
  customer: specialize TArray<specialize TFPGMap<string, integer>>;
  end_date: string;
  l: specialize TFPGMap<string, Variant>;
  lineitem: specialize TArray<specialize TFPGMap<string, Variant>>;
  n1: specialize TFPGMap<string, Variant>;
  n2: specialize TFPGMap<string, Variant>;
  nation: specialize TArray<specialize TFPGMap<string, Variant>>;
  nation1: string;
  nation2: string;
  o: specialize TFPGMap<string, integer>;
  orders: specialize TArray<specialize TFPGMap<string, integer>>;
  _result: specialize TArray<specialize TFPGMap<string, Variant>>;
  s: specialize TFPGMap<string, integer>;
  start_date: string;
  supplier: specialize TArray<specialize TFPGMap<string, integer>>;
  x: integer;

procedure test_Q7_computes_revenue_between_FRANCE_and_GERMANY_by_year;
var
  _tmp11: specialize TFPGMap<string, Variant>;
begin
  _tmp11 := specialize TFPGMap<string, Variant>.Create;
  _tmp11.AddOrSetData('supp_nation', 'FRANCE');
  _tmp11.AddOrSetData('cust_nation', 'GERMANY');
  _tmp11.AddOrSetData('l_year', '1995');
  _tmp11.AddOrSetData('revenue', 900);
  if not ((_result = specialize TArray<_>([_tmp11]))) then raise Exception.Create('expect failed');
end;

begin
  _tmp0 := specialize TFPGMap<string, Variant>.Create;
  _tmp0.AddOrSetData('n_nationkey', 1);
  _tmp0.AddOrSetData('n_name', 'FRANCE');
  _tmp1 := specialize TFPGMap<string, Variant>.Create;
  _tmp1.AddOrSetData('n_nationkey', 2);
  _tmp1.AddOrSetData('n_name', 'GERMANY');
  nation := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp0, _tmp1]);
  _tmp2 := specialize TFPGMap<string, integer>.Create;
  _tmp2.AddOrSetData('s_suppkey', 100);
  _tmp2.AddOrSetData('s_nationkey', 1);
  supplier := specialize TArray<specialize TFPGMap<string, integer>>([_tmp2]);
  _tmp3 := specialize TFPGMap<string, integer>.Create;
  _tmp3.AddOrSetData('c_custkey', 200);
  _tmp3.AddOrSetData('c_nationkey', 2);
  customer := specialize TArray<specialize TFPGMap<string, integer>>([_tmp3]);
  _tmp4 := specialize TFPGMap<string, integer>.Create;
  _tmp4.AddOrSetData('o_orderkey', 1000);
  _tmp4.AddOrSetData('o_custkey', 200);
  orders := specialize TArray<specialize TFPGMap<string, integer>>([_tmp4]);
  _tmp5 := specialize TFPGMap<string, Variant>.Create;
  _tmp5.AddOrSetData('l_orderkey', 1000);
  _tmp5.AddOrSetData('l_suppkey', 100);
  _tmp5.AddOrSetData('l_extendedprice', 1000);
  _tmp5.AddOrSetData('l_discount', 0.1);
  _tmp5.AddOrSetData('l_shipdate', '1995-06-15');
  _tmp6 := specialize TFPGMap<string, Variant>.Create;
  _tmp6.AddOrSetData('l_orderkey', 1000);
  _tmp6.AddOrSetData('l_suppkey', 100);
  _tmp6.AddOrSetData('l_extendedprice', 800);
  _tmp6.AddOrSetData('l_discount', 0.05);
  _tmp6.AddOrSetData('l_shipdate', '1997-01-01');
  lineitem := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp5, _tmp6]);
  start_date := '1995-01-01';
  end_date := '1996-12-31';
  nation1 := 'FRANCE';
  nation2 := 'GERMANY';
  SetLength(_tmp7, 0);
  SetLength(_tmp8, 0);
  for l in lineitem do
  begin
    for o in orders do
    begin
      if not ((o.KeyData['o_orderkey'] = l.KeyData['l_orderkey'])) then continue;
      for c in customer do
      begin
        if not ((c.KeyData['c_custkey'] = o.KeyData['o_custkey'])) then continue;
        for s in supplier do
        begin
          if not ((s.KeyData['s_suppkey'] = l.KeyData['l_suppkey'])) then continue;
          for n1 in nation do
          begin
            if not ((n1.KeyData['n_nationkey'] = s.KeyData['s_nationkey'])) then continue;
            for n2 in nation do
            begin
              if not ((n2.KeyData['n_nationkey'] = c.KeyData['c_nationkey'])) then continue;
              if not (((((l.KeyData['l_shipdate'] >= start_date) and (l.KeyData['l_shipdate'] <= end_date)) and ((n1.KeyData['n_name'] = nation1) and (n2.KeyData['n_name'] = nation2))) or ((n1.KeyData['n_name'] = nation2) and (n2.KeyData['n_name'] = nation1)))) then continue;
              _tmp9 := specialize TFPGMap<string, Variant>.Create;
              _tmp9.AddOrSetData('supp_nation', g.key.supp_nation);
              _tmp9.AddOrSetData('cust_nation', g.key.cust_nation);
              _tmp9.AddOrSetData('l_year', g.key.l_year);
              SetLength(_tmp10, 0);
              for x in g do
              begin
                _tmp10 := Concat(_tmp10, [x.l.l_extendedprice * 1 - x.l.l_discount]);
              end;
              _tmp9.AddOrSetData('revenue', specialize _sumList<Variant>(_tmp10));
              _tmp7 := Concat(_tmp7, [_tmp9]);
              _tmp8 := Concat(_tmp8, [specialize TArray<specialize TFPGMap<string, Variant>>([supp_nation, cust_nation, l_year])]);
            end;
          end;
        end;
      end;
    end;
  end;
  specialize _sortBy<specialize TFPGMap<string, Variant>>(_tmp7, _tmp8);
  _result := _tmp7;
  specialize _json<specialize TArray<specialize TFPGMap<string, Variant>>>(_result);
  test_Q7_computes_revenue_between_FRANCE_and_GERMANY_by_year;
end.
