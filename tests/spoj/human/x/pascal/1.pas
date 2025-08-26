{ Solution for SPOJ TEST - Life, the Universe, and Everything
  https://www.spoj.com/problems/TEST }
program Test;
var
  n: integer;
begin
  while not eof do
  begin
    readln(n);
    if n = 42 then
      break;
    writeln(n);
  end;
end.
