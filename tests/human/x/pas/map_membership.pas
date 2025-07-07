program MapMembership;

function HasKey(k: string): boolean;
begin
  if (k = 'a') or (k = 'b') then
    HasKey := True
  else
    HasKey := False;
end;

begin
  Writeln(HasKey('a'));
  Writeln(HasKey('c'));
end.
