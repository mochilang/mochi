program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TFPGMap<Variant, Variant>;
  _tmp1: specialize TFPGMap<Variant, Variant>;
  _tmp2: specialize TFPGMap<Variant, integer>;
  _tmp3: specialize TFPGMap<Variant, integer>;
  _tmp4: specialize TFPGMap<Variant, Variant>;
  _tmp5: specialize TArray<specialize TFPGMap<string, Variant>>;
  customers: specialize TArray<specialize TFPGMap<string, Variant>>;
  entry: specialize TFPGMap<string, Variant>;
  o: specialize TFPGMap<string, integer>;
  orders: specialize TArray<specialize TFPGMap<string, integer>>;
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
  _tmp2.AddOrSetData('total', 250);
  _tmp3 := specialize TFPGMap<Variant, integer>.Create;
  _tmp3.AddOrSetData('id', 101);
  _tmp3.AddOrSetData('customerId', 3);
  _tmp3.AddOrSetData('total', 80);
  orders := specialize TArray<specialize TFPGMap<string, integer>>([_tmp2, _tmp3]);
  _tmp4 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp4.AddOrSetData('orderId', o.id);
  _tmp4.AddOrSetData('customer', c);
  _tmp4.AddOrSetData('total', o.total);
  SetLength(_tmp5, 0);
  for o in orders do
    begin
      for c in customers do
        begin
          if not ((o.customerId = c.id)) then continue;
          _tmp5 := Concat(_tmp5, [_tmp4]);
        end;
    end;
  _result := _tmp5;
  writeln('--- Left Join ---');
  for entry in _result do
    begin
      writeln('Order', ' ', entry.orderId, ' ', 'customer', ' ', entry.customer, ' ', 'total', ' ',
              entry.total);
    end;
end.
