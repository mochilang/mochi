program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

  generic function _sliceList<T>(arr: specialize TArray<T>; i, j: integer): specialize TArray<T>;

var start_, end_, n: integer;
begin
  start_ := i;
  end_ := j;
  n := Length(arr);
  if start_ < 0 then start_ := n + start_;
  if end_ < 0 then end_ := n + end_;
  if start_ < 0 then start_ := 0;
  if end_ > n then end_ := n;
  if end_ < start_ then end_ := start_;
  Result := Copy(arr, start_ + 1, end_ - start_);
end;

generic procedure _sortBy<T>(var arr: specialize TArray<T>; keys: specialize TArray<Variant>);

var i,j: integer;
  tmp: T;
  k: Variant;
begin
  for i := 0 to High(arr) - 1 do
    for j := i + 1 to High(arr) do
      if keys[i] > keys[j] then
        begin
          tmp := arr[i];
          arr[i] := arr[j];
          arr[j] := tmp;
          k := keys[i];
          keys[i] := keys[j];
          keys[j] := k;
        end;
end;

var
  _tmp0: specialize TFPGMap<Variant, Variant>;
  _tmp1: specialize TFPGMap<Variant, Variant>;
  _tmp10: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp2: specialize TFPGMap<Variant, Variant>;
  _tmp3: specialize TFPGMap<Variant, Variant>;
  _tmp4: specialize TFPGMap<Variant, Variant>;
  _tmp5: specialize TFPGMap<Variant, Variant>;
  _tmp6: specialize TFPGMap<Variant, Variant>;
  _tmp7: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp8: specialize TArray<Variant>;
  _tmp9: specialize TArray<specialize TFPGMap<string, Variant>>;
  expensive: specialize TArray<specialize TFPGMap<string, Variant>>;
  item: specialize TFPGMap<string, Variant>;
  p: specialize TFPGMap<string, Variant>;
  products: specialize TArray<specialize TFPGMap<string, Variant>>;

begin
  _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp0.AddOrSetData('name', 'Laptop');
  _tmp0.AddOrSetData('price', 1500);
  _tmp1 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp1.AddOrSetData('name', 'Smartphone');
  _tmp1.AddOrSetData('price', 900);
  _tmp2 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp2.AddOrSetData('name', 'Tablet');
  _tmp2.AddOrSetData('price', 600);
  _tmp3 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp3.AddOrSetData('name', 'Monitor');
  _tmp3.AddOrSetData('price', 300);
  _tmp4 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp4.AddOrSetData('name', 'Keyboard');
  _tmp4.AddOrSetData('price', 100);
  _tmp5 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp5.AddOrSetData('name', 'Mouse');
  _tmp5.AddOrSetData('price', 50);
  _tmp6 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp6.AddOrSetData('name', 'Headphones');
  _tmp6.AddOrSetData('price', 200);
  products := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp0, _tmp1, _tmp2, _tmp3,
              _tmp4, _tmp5, _tmp6]);
  SetLength(_tmp7, 0);
  SetLength(_tmp8, 0);
  for p in products do
    begin
      _tmp7 := Concat(_tmp7, [p]);
      _tmp8 := Concat(_tmp8, [-p.price]);
    end;
  specialize _sortBy<specialize TFPGMap<string, Variant>>(_tmp7, _tmp8);
  _tmp9 := specialize _sliceList<specialize TFPGMap<string, Variant>>(_tmp7, 1, Length(_tmp7));
  _tmp10 := specialize _sliceList<specialize TFPGMap<string, Variant>>(_tmp9, 0, 3);
  expensive := _tmp10;
  writeln('--- Top products (excluding most expensive) ---');
  for item in expensive do
    begin
      writeln(item.name, ' ', 'costs $', ' ', item.price);
    end;
end.
