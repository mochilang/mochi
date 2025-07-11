program LeftJoinMulti;
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
  _tmp4: specialize TFPGMap<string, Variant>;
  _tmp5: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp6: boolean;
  _tmp7: specialize TFPGMap<string, Variant>;
  _tmp8: specialize TFPGMap<string, Variant>;
  customers: specialize TArray<specialize TFPGMap<string, Variant>>;
  items: specialize TArray<specialize TFPGMap<string, Variant>>;
  o: specialize TFPGMap<string, integer>;
  orders: specialize TArray<specialize TFPGMap<string, integer>>;
  r: specialize TFPGMap<string, Variant>;
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
  _tmp3 := specialize TFPGMap<string, integer>.Create;
  _tmp3.AddOrSetData('id', 101);
  _tmp3.AddOrSetData('customerId', 2);
  orders := specialize TArray<specialize TFPGMap<string, integer>>([_tmp2, _tmp3]);
  _tmp4 := specialize TFPGMap<string, Variant>.Create;
  _tmp4.AddOrSetData('orderId', 100);
  _tmp4.AddOrSetData('sku', 'a');
  items := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp4]);
  SetLength(_tmp5, 0);
  for o in orders do
    begin
      for c in customers do
        begin
          if not ((o.KeyData['customerId'] = c.id)) then continue;
          _tmp6 := False;
          for i in items do
            begin
              if not ((o.KeyData['id'] = i.orderId)) then continue;
              _tmp6 := True;
              _tmp7 := specialize TFPGMap<string, Variant>.Create;
              _tmp7.AddOrSetData('orderId', o.KeyData['id']);
              _tmp7.AddOrSetData('name', c.KeyData['name']);
              _tmp7.AddOrSetData('item', i);
              _tmp5 := Concat(_tmp5, [_tmp7]);
            end;
          if not _tmp6 then
            begin
              i := nil;
              _tmp8 := specialize TFPGMap<string, Variant>.Create;
              _tmp8.AddOrSetData('orderId', o.KeyData['id']);
              _tmp8.AddOrSetData('name', c.KeyData['name']);
              _tmp8.AddOrSetData('item', i);
              _tmp5 := Concat(_tmp5, [_tmp8]);
            end;
        end;
    end;
  _result := _tmp5;
  writeln('--- Left Join Multi ---');
  for r in _result do
    begin
      writeln(r.KeyData['orderId'], ' ', r.KeyData['name'], ' ', r.KeyData['item']);
    end;
end.
