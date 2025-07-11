program GroupByLeftJoin;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

  generic function _countList<T>(arr: specialize TArray<T>): integer;
begin
  Result := Length(arr);
end;

var
  _tmp0: specialize TFPGMap<string, Variant>;
  _tmp1: specialize TFPGMap<string, Variant>;
  _tmp10: specialize TFPGMap<string, Variant>;
  _tmp11: specialize TArray<Variant>;
  _tmp2: specialize TFPGMap<string, Variant>;
  _tmp3: specialize TFPGMap<string, integer>;
  _tmp4: specialize TFPGMap<string, integer>;
  _tmp5: specialize TFPGMap<string, integer>;
  _tmp6: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp7: boolean;
  _tmp8: specialize TFPGMap<string, Variant>;
  _tmp9: specialize TArray<Variant>;
  c: specialize TFPGMap<string, Variant>;
  customers: specialize TArray<specialize TFPGMap<string, Variant>>;
  orders: specialize TArray<specialize TFPGMap<string, integer>>;
  r: integer;
  s: specialize TFPGMap<string, Variant>;
  stats: specialize TArray<specialize TFPGMap<string, Variant>>;

begin
  _tmp0 := specialize TFPGMap<string, Variant>.Create;
  _tmp0.AddOrSetData('id', 1);
  _tmp0.AddOrSetData('name', 'Alice');
  _tmp1 := specialize TFPGMap<string, Variant>.Create;
  _tmp1.AddOrSetData('id', 2);
  _tmp1.AddOrSetData('name', 'Bob');
  _tmp2 := specialize TFPGMap<string, Variant>.Create;
  _tmp2.AddOrSetData('id', 3);
  _tmp2.AddOrSetData('name', 'Charlie');
  customers := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp0, _tmp1, _tmp2]);
  _tmp3 := specialize TFPGMap<string, integer>.Create;
  _tmp3.AddOrSetData('id', 100);
  _tmp3.AddOrSetData('customerId', 1);
  _tmp4 := specialize TFPGMap<string, integer>.Create;
  _tmp4.AddOrSetData('id', 101);
  _tmp4.AddOrSetData('customerId', 1);
  _tmp5 := specialize TFPGMap<string, integer>.Create;
  _tmp5.AddOrSetData('id', 102);
  _tmp5.AddOrSetData('customerId', 2);
  orders := specialize TArray<specialize TFPGMap<string, integer>>([_tmp3, _tmp4, _tmp5]);
  SetLength(_tmp6, 0);
  for c in customers do
    begin
      _tmp7 := False;
      for o in orders do
        begin
          if not ((o.customerId = c.KeyData['id'])) then continue;
          _tmp7 := True;
          _tmp8 := specialize TFPGMap<string, Variant>.Create;
          _tmp8.AddOrSetData('name', g.key);
          SetLength(_tmp9, 0);
          for r in g do
            begin
              if not (r.o) then continue;
              _tmp9 := Concat(_tmp9, [r]);
            end;
          _tmp8.AddOrSetData('count', specialize _countList<Variant>(_tmp9));
          _tmp6 := Concat(_tmp6, [_tmp8]);
        end;
      if not _tmp7 then
        begin
          o := nil;
          _tmp10 := specialize TFPGMap<string, Variant>.Create;
          _tmp10.AddOrSetData('name', g.key);
          SetLength(_tmp11, 0);
          for r in g do
            begin
              if not (r.o) then continue;
              _tmp11 := Concat(_tmp11, [r]);
            end;
          _tmp10.AddOrSetData('count', specialize _countList<Variant>(_tmp11));
          _tmp6 := Concat(_tmp6, [_tmp10]);
        end;
    end;
  stats := _tmp6;
  writeln('--- Group Left Join ---');
  for s in stats do
    begin
      writeln(s.KeyData['name'], ' ', 'orders:', ' ', s.KeyData['count']);
    end;
end.
