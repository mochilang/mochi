program RecordAssign;

type
  Counter = record
    n: integer;
  end;

procedure incCounter(var c: Counter);
begin
  c.n := c.n + 1;
end;

var
  c: Counter;
begin
  c.n := 0;
  incCounter(c);
  Writeln(c.n);
end.
