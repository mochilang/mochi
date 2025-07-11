program OuterJoin;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TFPGMap<string, Variant>;
  _tmp1: specialize TFPGMap<string, Variant>;
  _tmp2: specialize TFPGMap<string, Variant>;
  _tmp3: specialize TFPGMap<string, Variant>;
  _tmp4: specialize TFPGMap<string, integer>;
  _tmp5: specialize TFPGMap<string, integer>;
  _tmp6: specialize TFPGMap<string, integer>;
  _tmp7: specialize TFPGMap<string, integer>;
  _tmp8: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp9: specialize TFPGMap<string, Variant>;
  customers: specialize TArray<specialize TFPGMap<string, Variant>>;
  o: specialize TFPGMap<string, integer>;
  orders: specialize TArray<specialize TFPGMap<string, integer>>;
  _result: specialize TArray<specialize TFPGMap<string, Variant>>;
  row: specialize TFPGMap<string, Variant>;

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
  _tmp3 := specialize TFPGMap<string, Variant>.Create;
  _tmp3.AddOrSetData('id', 4);
  _tmp3.AddOrSetData('name', 'Diana');
  customers := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp0, _tmp1, _tmp2, _tmp3]);
  _tmp4 := specialize TFPGMap<string, integer>.Create;
  _tmp4.AddOrSetData('id', 100);
  _tmp4.AddOrSetData('customerId', 1);
  _tmp4.AddOrSetData('total', 250);
  _tmp5 := specialize TFPGMap<string, integer>.Create;
  _tmp5.AddOrSetData('id', 101);
  _tmp5.AddOrSetData('customerId', 2);
  _tmp5.AddOrSetData('total', 125);
  _tmp6 := specialize TFPGMap<string, integer>.Create;
  _tmp6.AddOrSetData('id', 102);
  _tmp6.AddOrSetData('customerId', 1);
  _tmp6.AddOrSetData('total', 300);
  _tmp7 := specialize TFPGMap<string, integer>.Create;
  _tmp7.AddOrSetData('id', 103);
  _tmp7.AddOrSetData('customerId', 5);
  _tmp7.AddOrSetData('total', 80);
  orders := specialize TArray<specialize TFPGMap<string, integer>>([_tmp4, _tmp5, _tmp6, _tmp7]);
  SetLength(_tmp8, 0);
  for o in orders do
    begin
      for c in customers do
        begin
          if not ((o.KeyData['customerId'] = c.id)) then continue;
          _tmp9 := specialize TFPGMap<string, Variant>.Create;
          _tmp9.AddOrSetData('order', o);
          _tmp9.AddOrSetData('customer', c);
          _tmp8 := Concat(_tmp8, [_tmp9]);
        end;
    end;
  _result := _tmp8;
  writeln('--- Outer Join using syntax ---');
  for row in _result do
    begin
      if row.KeyData['order'] then
        begin
          if row.KeyData['customer'] then
            begin
              writeln('Order', ' ', row.KeyData['order'].id, ' ', 'by', ' ', row.KeyData['customer']
                      .name, ' ', '- $', ' ', row.KeyData['order'].total);
            end
          else
            begin
              writeln('Order', ' ', row.KeyData['order'].id, ' ', 'by', ' ', 'Unknown', ' ', '- $',
                      ' ', row.KeyData['order'].total);
            end;
        end
      else
        begin
          writeln('Customer', ' ', row.KeyData['customer'].name, ' ', 'has no orders');
        end;
    end;
end.
