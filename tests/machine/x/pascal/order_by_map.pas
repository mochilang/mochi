program OrderByMap;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

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

var
  _tmp0: specialize TFPGMap<string, integer>;
  _tmp1: specialize TFPGMap<string, integer>;
  _tmp2: specialize TFPGMap<string, integer>;
  _tmp3: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp4: specialize TArray<Variant>;
  _tmp5: specialize TFPGMap<string, integer>;
  data: specialize TArray<specialize TFPGMap<string, integer>>;
  sorted: specialize TArray<specialize TFPGMap<string, integer>>;
  x: specialize TFPGMap<string, integer>;

begin
  _tmp0 := specialize TFPGMap<string, integer>.Create;
  _tmp0.AddOrSetData('a', 1);
  _tmp0.AddOrSetData('b', 2);
  _tmp1 := specialize TFPGMap<string, integer>.Create;
  _tmp1.AddOrSetData('a', 1);
  _tmp1.AddOrSetData('b', 1);
  _tmp2 := specialize TFPGMap<string, integer>.Create;
  _tmp2.AddOrSetData('a', 0);
  _tmp2.AddOrSetData('b', 5);
  data := specialize TArray<specialize TFPGMap<string, integer>>([_tmp0, _tmp1, _tmp2]);
  SetLength(_tmp3, 0);
  SetLength(_tmp4, 0);
  for x in data do
    begin
      _tmp3 := Concat(_tmp3, [x]);
      _tmp5 := specialize TFPGMap<string, integer>.Create;
      _tmp5.AddOrSetData('a', x.KeyData['a']);
      _tmp5.AddOrSetData('b', x.KeyData['b']);
      _tmp4 := Concat(_tmp4, [_tmp5]);
    end;
  specialize _sortBy<specialize TFPGMap<string, Variant>>(_tmp3, _tmp4);
  sorted := _tmp3;
  specialize _printList<specialize TFPGMap<string, integer>>(sorted);
end.
