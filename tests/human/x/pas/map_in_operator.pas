program MapInOperator;

function HasKey(k: integer): boolean;
begin
  HasKey := (k = 1) or (k = 2);
end;

begin
  Writeln(HasKey(1));
  Writeln(HasKey(3));
end.
