program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TFPGMap<Variant, Variant>;
  _tmp1: specialize TFPGMap<Variant, Variant>;
  _tmp2: specialize TFPGMap<Variant, Variant>;
  _tmp3: specialize TFPGMap<Variant, integer>;
  _tmp4: specialize TFPGMap<Variant, integer>;
  _tmp5: specialize TFPGMap<Variant, integer>;
  _tmp6: specialize TFPGMap<Variant, Variant>;
  _tmp7: specialize TArray<specialize TFPGMap<string, Variant>>;
  c: specialize TFPGMap<string, Variant>;
  customers: specialize TArray<specialize TFPGMap<string, Variant>>;
  entry: specialize TFPGMap<string, integer>;
  o: specialize TFPGMap<string, integer>;
  orders: specialize TArray<specialize TFPGMap<string, integer>>;
  _result: specialize TArray<specialize TFPGMap<string, integer>>;

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
  _tmp3.AddOrSetData('total', 250);
  _tmp4 := specialize TFPGMap<Variant, integer>.Create;
  _tmp4.AddOrSetData('id', 101);
  _tmp4.AddOrSetData('customerId', 2);
  _tmp4.AddOrSetData('total', 125);
  _tmp5 := specialize TFPGMap<Variant, integer>.Create;
  _tmp5.AddOrSetData('id', 102);
  _tmp5.AddOrSetData('customerId', 1);
  _tmp5.AddOrSetData('total', 300);
  orders := specialize TArray<specialize TFPGMap<string, integer>>([_tmp3, _tmp4, _tmp5]);
  _tmp6 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp6.AddOrSetData('orderId', o.id);
  _tmp6.AddOrSetData('orderCustomerId', o.customerId);
  _tmp6.AddOrSetData('pairedCustomerName', c.name);
  _tmp6.AddOrSetData('orderTotal', o.total);
  SetLength(_tmp7, 0);
  for o in orders do
    begin
      for c in customers do
        begin
          _tmp7 := Concat(_tmp7, [_tmp6]);
        end;
    end;
  _result := _tmp7;
  writeln('--- Cross Join: All order-customer pairs ---');
  for entry in _result do
    begin
      writeln('Order', ' ', entry.orderId, ' ', '(customerId:', ' ', entry.orderCustomerId, ' ',
              ', total: $', ' ', entry.orderTotal, ' ', ') paired with', ' ', entry.
              pairedCustomerName);
    end;
end.
