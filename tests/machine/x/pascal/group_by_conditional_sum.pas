program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

  generic procedure _printList<T>(arr: specialize TArray<T>);

var i: Integer;
begin
  for i := 0 to High(arr) do
    begin
      if i > 0 then Write(' ');
      Write(arr[i]);
    end;
  writeln();
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

generic function _sumList<T>(arr: specialize TArray<T>): double;

var i: integer;
  s: double;
begin
  s := 0;
  for i := 0 to High(arr) do
    s := s + arr[i];
  Result := s;
end;

var
  _tmp0: specialize TFPGMap<Variant, Variant>;
  _tmp1: specialize TFPGMap<Variant, Variant>;
  _tmp2: specialize TFPGMap<Variant, Variant>;
  _tmp3: specialize TFPGMap<Variant, Variant>;
  _tmp4: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp5: specialize TArray<Variant>;
  _tmp6: specialize TArray<Variant>;
  _tmp7: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp8: specialize TArray<Variant>;
  i: specialize TFPGMap<string, Variant>;
  items: specialize TArray<specialize TFPGMap<string, Variant>>;
  _result: specialize TArray<specialize TFPGMap<string, Variant>>;
  x: integer;

begin
  _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp0.AddOrSetData('cat', 'a');
  _tmp0.AddOrSetData('val', 10);
  _tmp0.AddOrSetData('flag', True);
  _tmp1 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp1.AddOrSetData('cat', 'a');
  _tmp1.AddOrSetData('val', 5);
  _tmp1.AddOrSetData('flag', False);
  _tmp2 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp2.AddOrSetData('cat', 'b');
  _tmp2.AddOrSetData('val', 20);
  _tmp2.AddOrSetData('flag', True);
  items := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp0, _tmp1, _tmp2]);
  _tmp3 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp3.AddOrSetData('cat', g.key);
  if x.flag then
    begin
      _tmp4 := x.val;
    end
  else
    begin
      _tmp4 := 0;
    end;
  SetLength(_tmp5, 0);
  for x in g do
    begin
      _tmp5 := Concat(_tmp5, [_tmp4]);
    end;
  SetLength(_tmp6, 0);
  for x in g do
    begin
      _tmp6 := Concat(_tmp6, [x.val]);
    end;
  _tmp3.AddOrSetData('share', specialize _sumList<Variant>(_tmp5) div specialize _sumList<Variant>(
                                                                                               _tmp6
  ));
  SetLength(_tmp7, 0);
  SetLength(_tmp8, 0);
  for i in items do
    begin
      _tmp7 := Concat(_tmp7, [_tmp3]);
      _tmp8 := Concat(_tmp8, [g.key]);
    end;
  specialize _sortBy<specialize TFPGMap<string, Variant>>(_tmp7, _tmp8);
  _result := _tmp7;
  specialize _printList<specialize TFPGMap<string, Variant>>(_result);
end.
