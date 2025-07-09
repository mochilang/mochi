program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

  generic function _countList<T>(arr: specialize TArray<T>): integer;
begin
  Result := Length(arr);
end;

var
  _tmp0: specialize TFPGMap<Variant, Variant>;
  _tmp1: specialize TFPGMap<Variant, Variant>;
  _tmp2: specialize TFPGMap<Variant, Variant>;
  _tmp3: specialize TFPGMap<Variant, integer>;
  _tmp4: specialize TFPGMap<Variant, integer>;
  _tmp5: specialize TFPGMap<Variant, integer>;
  _tmp6: specialize TFPGMap<Variant, Variant>;
  _tmp7: specialize TArray<Variant>;
  _tmp8: specialize TArray<specialize TFPGMap<string, Variant>>;
  c: specialize TFPGMap<string, Variant>;
  customers: specialize TArray<specialize TFPGMap<string, Variant>>;
  orders: specialize TArray<specialize TFPGMap<string, integer>>;
  r: integer;
  s: specialize TFPGMap<string, Variant>;
  stats: specialize TArray<specialize TFPGMap<string, Variant>>;

begin
  _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp0.AddOrSetData('id', 1);
  _tmp0.AddOrSetData('name', 'Alice');
  _tmp1 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp1.AddOrSetData('id', 2);
  _tmp1.AddOrSetData('name', 'Bob');
  _tmp2 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp2.AddOrSetData('id', 3);
  _tmp2.AddOrSetData('name', 'Charlie');
  customers := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp0, _tmp1, _tmp2]);
  _tmp3 := specialize TFPGMap<Variant, integer>.Create;
  _tmp3.AddOrSetData('id', 100);
  _tmp3.AddOrSetData('customerId', 1);
  _tmp4 := specialize TFPGMap<Variant, integer>.Create;
  _tmp4.AddOrSetData('id', 101);
  _tmp4.AddOrSetData('customerId', 1);
  _tmp5 := specialize TFPGMap<Variant, integer>.Create;
  _tmp5.AddOrSetData('id', 102);
  _tmp5.AddOrSetData('customerId', 2);
  orders := specialize TArray<specialize TFPGMap<string, integer>>([_tmp3, _tmp4, _tmp5]);
  _tmp6 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp6.AddOrSetData('name', g.key);
  SetLength(_tmp7, 0);
  for r in g do
    begin
      if not (r.o) then continue;
      _tmp7 := Concat(_tmp7, [r]);
    end;
  _tmp6.AddOrSetData('count', specialize _countList<Variant>(_tmp7));
  SetLength(_tmp8, 0);
  for c in customers do
    begin
      for o in orders do
        begin
          if not ((o.customerId = c.id)) then continue;
          _tmp8 := Concat(_tmp8, [_tmp6]);
        end;
    end;
  stats := _tmp8;
  writeln('--- Group Left Join ---');
  for s in stats do
    begin
      writeln(s.name, ' ', 'orders:', ' ', s.count);
    end;
end.
