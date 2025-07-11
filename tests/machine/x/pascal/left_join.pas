program LeftJoin;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TFPGMap<string, Variant>;
  _tmp1: specialize TFPGMap<string, Variant>;
  _tmp2: specialize TFPGMap<string, integer>;
  _tmp3: specialize TFPGMap<string, integer>;
  _tmp4: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp5: boolean;
  _tmp6: specialize TFPGMap<string, Variant>;
  _tmp7: specialize TFPGMap<string, Variant>;
  customers: specialize TArray<specialize TFPGMap<string, Variant>>;
  entry: specialize TFPGMap<string, Variant>;
  o: specialize TFPGMap<string, integer>;
  orders: specialize TArray<specialize TFPGMap<string, integer>>;
  _result: specialize TArray<specialize TFPGMap<string, Variant>>;

begin
  _tmp0 := specialize TFPGMap<string, Variant>.Create;
  _tmp0.AddOrSetData('id', 1);
  _tmp0.AddOrSetData('name', 'Alice');
  _tmp1 := specialize TFPGMap<string, Variant>.Create;
  _tmp1.AddOrSetData('id', 2);
  _tmp1.AddOrSetData('name', 'Bob');
  customers := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp0, _tmp1]);
  _tmp2 := specialize TFPGMap<string, integer>.Create;
  _tmp2.AddOrSetData('id', 100);
  _tmp2.AddOrSetData('customerId', 1);
  _tmp2.AddOrSetData('total', 250);
  _tmp3 := specialize TFPGMap<string, integer>.Create;
  _tmp3.AddOrSetData('id', 101);
  _tmp3.AddOrSetData('customerId', 3);
  _tmp3.AddOrSetData('total', 80);
  orders := specialize TArray<specialize TFPGMap<string, integer>>([_tmp2, _tmp3]);
  SetLength(_tmp4, 0);
  for o in orders do
    begin
      _tmp5 := False;
      for c in customers do
        begin
          if not ((o.KeyData['customerId'] = c.id)) then continue;
          _tmp5 := True;
          _tmp6 := specialize TFPGMap<string, Variant>.Create;
          _tmp6.AddOrSetData('orderId', o.KeyData['id']);
          _tmp6.AddOrSetData('customer', c);
          _tmp6.AddOrSetData('total', o.KeyData['total']);
          _tmp4 := Concat(_tmp4, [_tmp6]);
        end;
      if not _tmp5 then
        begin
          c := nil;
          _tmp7 := specialize TFPGMap<string, Variant>.Create;
          _tmp7.AddOrSetData('orderId', o.KeyData['id']);
          _tmp7.AddOrSetData('customer', c);
          _tmp7.AddOrSetData('total', o.KeyData['total']);
          _tmp4 := Concat(_tmp4, [_tmp7]);
        end;
    end;
  _result := _tmp4;
  writeln('--- Left Join ---');
  for entry in _result do
    begin
      writeln('Order', ' ', entry.KeyData['orderId'], ' ', 'customer', ' ', entry.KeyData['customer'
              ], ' ', 'total', ' ', entry.KeyData['total']);
    end;
end.
