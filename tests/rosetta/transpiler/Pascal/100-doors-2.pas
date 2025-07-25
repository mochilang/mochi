{$mode objfpc}
program Main;
uses SysUtils;
var
  door: integer;
  incrementer: integer;
  current: integer;
  line: string;
begin
  door := 1;
  incrementer := 0;
  for current := 1 to (101 - 1) do begin
  line := ('Door ' + IntToStr(current)) + ' ';
  if current = door then begin
  line := line + 'Open';
  incrementer := incrementer + 1;
  door := (door + (2 * incrementer)) + 1;
end else begin
  line := line + 'Closed';
end;
  writeln(line);
end;
end.
