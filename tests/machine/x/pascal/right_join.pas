program RightJoin;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TFPGMap<string, Variant>;
  _tmp1: specialize TFPGMap<string, Variant>;
  _tmp10: specialize TFPGMap<string, Variant>;
  _tmp2: specialize TFPGMap<string, Variant>;
  _tmp3: specialize TFPGMap<string, Variant>;
  _tmp4: specialize TFPGMap<string, integer>;
  _tmp5: specialize TFPGMap<string, integer>;
  _tmp6: specialize TFPGMap<string, integer>;
  _tmp7: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp8: boolean;
  _tmp9: specialize TFPGMap<string, Variant>;
  customers: specialize TArray<specialize TFPGMap<string, Variant>>;
  entry: specialize TFPGMap<string, Variant>;
  o: specialize TFPGMap<string, Variant>;
  orders: specialize TArray<specialize TFPGMap<string, integer>>;
  _result: specialize TArray<specialize TFPGMap<string, Variant>>;

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
  orders := specialize TArray<specialize TFPGMap<string, integer>>([_tmp4, _tmp5, _tmp6]);
  SetLength(_tmp7, 0);
  for o in orders do
    begin
      _tmp8 := False;
      for c in customers do
        begin
          _tmp8 := True;
          for o in orders do
            begin
              for c in customers do
                begin
                  if not ((o.customerId = c.KeyData['id'])) then continue;
                  _tmp9 := specialize TFPGMap<string, Variant>.Create;
                  _tmp9.AddOrSetData('customerName', c.KeyData['name']);
                  _tmp9.AddOrSetData('order', o);
                  _tmp7 := Concat(_tmp7, [_tmp9]);
                end;
            end;
        end;
      if not _tmp8 then
        begin
          c := nil;
          for o in orders do
            begin
              for c in customers do
                begin
                  if not ((o.customerId = c.KeyData['id'])) then continue;
                  _tmp10 := specialize TFPGMap<string, Variant>.Create;
                  _tmp10.AddOrSetData('customerName', c.KeyData['name']);
                  _tmp10.AddOrSetData('order', o);
                  _tmp7 := Concat(_tmp7, [_tmp10]);
                end;
            end;
        end;
    end;
  _result := _tmp7;
  writeln('--- Right Join using syntax ---');
  for entry in _result do
    begin
      if entry.KeyData['order'] then
        begin
          writeln('Customer', ' ', entry.KeyData['customerName'], ' ', 'has order', ' ', entry.
                  KeyData['order'].id, ' ', '- $', ' ', entry.KeyData['order'].total);
        end
      else
        begin
          writeln('Customer', ' ', entry.KeyData['customerName'], ' ', 'has no orders');
        end;
    end;
end.
