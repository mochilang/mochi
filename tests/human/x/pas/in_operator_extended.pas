program InOperatorExtended;

function ContainsInt(arr: array of integer; value: integer): boolean;
var
  i: integer;
begin
  for i := Low(arr) to High(arr) do
    if arr[i] = value then
    begin
      ContainsInt := True;
      Exit;
    end;
  ContainsInt := False;
end;

function MapHasKey(key: string): boolean;
begin
  if key = 'a' then
    MapHasKey := True
  else
    MapHasKey := False;
end;

var
  xs: array[1..3] of integer = (1,2,3);
  ys: array of integer;
  count,i: integer;
  s: string;
begin
  count := 0;
  for i := 1 to Length(xs) do
    if xs[i] mod 2 = 1 then
    begin
      SetLength(ys, count+1);
      ys[count] := xs[i];
      Inc(count);
    end;

  Writeln(ContainsInt(ys, 1));
  Writeln(ContainsInt(ys, 2));

  Writeln(MapHasKey('a'));
  Writeln(MapHasKey('b'));

  s := 'hello';
  Writeln(Pos('ell', s) <> 0);
  Writeln(Pos('foo', s) <> 0);
end.
