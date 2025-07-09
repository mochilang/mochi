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
  _tmp4: specialize TFPGMap<Variant, Variant>;
  _tmp5: specialize TFPGMap<Variant, Variant>;
  _tmp6: specialize TArray<specialize TFPGMap<string, Variant>>;
  customers: specialize TArray<specialize TFPGMap<string, Variant>>;
  items: specialize TArray<specialize TFPGMap<string, Variant>>;
  o: specialize TFPGMap<string, integer>;
  orders: specialize TArray<specialize TFPGMap<string, integer>>;
  r: specialize TFPGMap<string, Variant>;
  _result: specialize TArray<specialize TFPGMap<string, Variant>>;

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
  _tmp3.AddOrSetData('customerId', 2);
  orders := specialize TArray<specialize TFPGMap<string, integer>>([_tmp2, _tmp3]);
  _tmp4 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp4.AddOrSetData('orderId', 100);
  _tmp4.AddOrSetData('sku', 'a');
  items := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp4]);
  _tmp5 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp5.AddOrSetData('orderId', o.id);
  _tmp5.AddOrSetData('name', c.name);
  _tmp5.AddOrSetData('item', i);
  SetLength(_tmp6, 0);
  for o in orders do
    begin
      for c in customers do
        begin
          if not ((o.customerId = c.id)) then continue;
          for i in items do
            begin
              if not ((o.id = i.orderId)) then continue;
              _tmp6 := Concat(_tmp6, [_tmp5]);
            end;
        end;
    end;
  _result := _tmp6;
  writeln('--- Left Join Multi ---');
  for r in _result do
    begin
      writeln(r.orderId, ' ', r.name, ' ', r.item);
    end;
end.
