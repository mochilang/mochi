program InOperatorExtended;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

  generic function _containsList<T>(arr: specialize TArray<T>; v: T): boolean;

var i: Integer;
begin
  Result := False;
  for i := 0 to High(arr) do
    if arr[i] = v then exit(True);
end;

var
  _tmp0: specialize TArray<integer>;
  _tmp1: specialize TFPGMap<string, integer>;
  m: specialize TFPGMap<string, integer>;
  s: string;
  x: integer;
  xs: specialize TArray<integer>;
  ys: specialize TArray<integer>;

begin
  xs := specialize TArray<integer>([1, 2, 3]);
  SetLength(_tmp0, 0);
  for x in xs do
    begin
      if not ((x mod 2 = 1)) then continue;
      _tmp0 := Concat(_tmp0, [x]);
    end;
  ys := _tmp0;
  writeln(specialize _containsList<integer>(ys, 1));
  writeln(specialize _containsList<integer>(ys, 2));
  _tmp1 := specialize TFPGMap<string, integer>.Create;
  _tmp1.AddOrSetData('a', 1);
  m := _tmp1;
  writeln((m.IndexOf('a') >= 0));
  writeln((m.IndexOf('b') >= 0));
  s := 'hello';
  writeln((Pos('ell', s) > 0));
  writeln((Pos('foo', s) > 0));
end.
