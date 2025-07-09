program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TFPGMap<Variant, Variant>;
  _tmp1: specialize TFPGMap<Variant, Variant>;
  _tmp2: specialize TFPGMap<Variant, integer>;
  _tmp3: specialize TFPGMap<Variant, integer>;
  _tmp4: specialize TFPGMap<Variant, integer>;
  _tmp5: specialize TFPGMap<Variant, Variant>;
  _tmp6: specialize TArray<specialize TFPGMap<string, Variant>>;
  customers: specialize TArray<specialize TFPGMap<string, Variant>>;
  o: specialize TFPGMap<string, integer>;
  orders: specialize TArray<specialize TFPGMap<string, integer>>;
  s: specialize TFPGMap<string, Variant>;
  stats: specialize TArray<specialize TFPGMap<string, Variant>>;

begin
  _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp0.AddOrSetData('id', 1);
  _tmp0.AddOrSetData('name', 'Alice');
  _tmp1 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp1.AddOrSetData('id', 2);
  _tmp1.AddOrSetData('name', 'Bob');
  customers := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp0, _tmp1]);
  _tmp2 := specialize TFPGMap<Variant, integer>.Create;
  _tmp2.AddOrSetData('id', 100);
  _tmp2.AddOrSetData('customerId', 1);
  _tmp3 := specialize TFPGMap<Variant, integer>.Create;
  _tmp3.AddOrSetData('id', 101);
  _tmp3.AddOrSetData('customerId', 1);
  _tmp4 := specialize TFPGMap<Variant, integer>.Create;
  _tmp4.AddOrSetData('id', 102);
  _tmp4.AddOrSetData('customerId', 2);
  orders := specialize TArray<specialize TFPGMap<string, integer>>([_tmp2, _tmp3, _tmp4]);
  _tmp5 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp5.AddOrSetData('name', g.key);
  _tmp5.AddOrSetData('count', Length(g));
  SetLength(_tmp6, 0);
  for o in orders do
    begin
      for c in customers do
        begin
          if not ((o.customerId = c.id)) then continue;
          _tmp6 := Concat(_tmp6, [_tmp5]);
        end;
    end;
  stats := _tmp6;
  writeln('--- Orders per customer ---');
  for s in stats do
    begin
      writeln(s.name, ' ', 'orders:', ' ', s.count);
    end;
end.
