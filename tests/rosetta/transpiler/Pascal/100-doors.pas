{$mode objfpc}
program Main;
var
  doors: array of boolean;
  i: integer;
  pass: integer;
  idx: integer;
  row: integer;
  line: string;
  col: integer;
begin
  doors := [];
  for i := 0 to (100 - 1) do begin
  doors := concat(doors, [false]);
end;
  for pass := 1 to (101 - 1) do begin
  idx := pass - 1;
  while idx < 100 do begin
  doors[idx] := not doors[idx];
  idx := idx + pass;
end;
end;
  for row := 0 to (10 - 1) do begin
  line := '';
  for col := 0 to (10 - 1) do begin
  idx := (row * 10) + col;
  if doors[idx] then begin
  line := line + '1';
end else begin
  line := line + '0';
end;
  if col < 9 then begin
  line := line + ' ';
end;
end;
  writeln(line);
end;
end.
