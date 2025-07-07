program BoolChain;
function boom: Boolean;
begin
  Writeln('boom');
  boom := True;
end;

begin
  Writeln((1 < 2) and (2 < 3) and (3 < 4));
  Writeln((1 < 2) and (2 > 3) and boom);
  Writeln((1 < 2) and (2 < 3) and (3 > 4) and boom);
end.
