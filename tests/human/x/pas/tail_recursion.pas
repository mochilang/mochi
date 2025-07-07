program TailRecursion;

function SumRec(n, acc: integer): integer;
begin
  if n = 0 then
    SumRec := acc
  else
    SumRec := SumRec(n - 1, acc + n);
end;

begin
  Writeln(SumRec(10, 0));
end.
