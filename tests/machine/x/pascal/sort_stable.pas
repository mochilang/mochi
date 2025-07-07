program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

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
  _tmp2: specialize TFPGMap<Variant, Variant>;
  _tmp3: specialize TArray<Variant>;
  _tmp4: specialize TArray<Variant>;
  i: specialize TFPGMap<string, Variant>;
  items: specialize TArray<specialize TFPGMap<string, Variant>>;
  _result: specialize TArray<Variant>;

begin
  _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp0.AddOrSetData('n', 1);
  _tmp0.AddOrSetData('v', 'a');
  _tmp1 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp1.AddOrSetData('n', 1);
  _tmp1.AddOrSetData('v', 'b');
  _tmp2 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp2.AddOrSetData('n', 2);
  _tmp2.AddOrSetData('v', 'c');
  items := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp0, _tmp1, _tmp2]);
  SetLength(_tmp3, 0);
  SetLength(_tmp4, 0);
  for i in items do
    begin
      _tmp3 := Concat(_tmp3, [i.v]);
      _tmp4 := Concat(_tmp4, [i.n]);
    end;
  specialize _sortBy<Variant>(_tmp3, _tmp4);
  _result := _tmp3;
  writeln(_result);
end.
