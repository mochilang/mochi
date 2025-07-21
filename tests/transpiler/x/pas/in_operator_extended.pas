{$mode objfpc}
program Main;
uses SysUtils;
function contains(xs: array of integer; v: integer): boolean;
var i: integer;
begin
  for i := 0 to High(xs) do begin
    if xs[i] = v then begin
      contains := true; exit;
    end;
  end;
  contains := false;
end;
type Anon29 = record
  a: integer;
end;
var
  xs: array of integer;
  ys: array of integer;
  m: Anon29;
  s: string;
  x: integer;
begin
  xs := [1, 2, 3];
  m := (a: 1);
  s := 'hello';
  ys := [];
  for x in xs do begin
  if (x mod 2) = 1 then begin
  ys := concat(ys, [x]);
end;
end;
  writeln(BoolToStr(contains(ys, 1), True));
  writeln(BoolToStr(contains(ys, 2), True));
  writeln(BoolToStr(Pos('a', m) <> 0, True));
  writeln(BoolToStr(Pos('b', m) <> 0, True));
  writeln(BoolToStr(Pos('ell', s) <> 0, True));
  writeln(BoolToStr(Pos('foo', s) <> 0, True));
end.
