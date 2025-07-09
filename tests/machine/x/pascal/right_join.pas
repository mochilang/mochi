program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TFPGMap<Variant, Variant>;
  _tmp1: specialize TFPGMap<Variant, Variant>;
  _tmp2: specialize TFPGMap<Variant, Variant>;
  _tmp3: specialize TFPGMap<Variant, Variant>;
  _tmp4: specialize TFPGMap<Variant, integer>;
  _tmp5: specialize TFPGMap<Variant, integer>;
  _tmp6: specialize TFPGMap<Variant, integer>;
  _tmp7: specialize TFPGMap<Variant, Variant>;
  _tmp8: specialize TArray<specialize TFPGMap<string, Variant>>;
  c: specialize TFPGMap<string, Variant>;
  customers: specialize TArray<specialize TFPGMap<string, Variant>>;
  entry: specialize TFPGMap<string, Variant>;
  orders: specialize TArray<specialize TFPGMap<string, integer>>;
  _result: specialize TArray<specialize TFPGMap<string, Variant>>;

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
  _tmp3 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp3.AddOrSetData('id', 4);
  _tmp3.AddOrSetData('name', 'Diana');
  customers := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp0, _tmp1, _tmp2, _tmp3]);
  _tmp4 := specialize TFPGMap<Variant, integer>.Create;
  _tmp4.AddOrSetData('id', 100);
  _tmp4.AddOrSetData('customerId', 1);
  _tmp4.AddOrSetData('total', 250);
  _tmp5 := specialize TFPGMap<Variant, integer>.Create;
  _tmp5.AddOrSetData('id', 101);
  _tmp5.AddOrSetData('customerId', 2);
  _tmp5.AddOrSetData('total', 125);
  _tmp6 := specialize TFPGMap<Variant, integer>.Create;
  _tmp6.AddOrSetData('id', 102);
  _tmp6.AddOrSetData('customerId', 1);
  _tmp6.AddOrSetData('total', 300);
  orders := specialize TArray<specialize TFPGMap<string, integer>>([_tmp4, _tmp5, _tmp6]);
  _tmp7 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp7.AddOrSetData('customerName', c.name);
  _tmp7.AddOrSetData('order', o);
  SetLength(_tmp8, 0);
  for c in customers do
    begin
      for o in orders do
        begin
          if not ((o.customerId = c.id)) then continue;
          _tmp8 := Concat(_tmp8, [_tmp7]);
        end;
    end;
  _result := _tmp8;
  writeln('--- Right Join using syntax ---');
  for entry in _result do
    begin
      if entry.order then
        begin
          writeln('Customer', ' ', entry.customerName, ' ', 'has order', ' ', entry.order.id, ' ',
                  '- $', ' ', entry.order.total);
        end
      else
        begin
          writeln('Customer', ' ', entry.customerName, ' ', 'has no orders');
        end;
    end;
end.
