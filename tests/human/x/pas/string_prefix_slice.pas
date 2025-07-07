program StringPrefixSlice;
var
  prefix, s1, s2: string;
begin
  prefix := 'fore';
  s1 := 'forest';
  Writeln(Copy(s1, 1, Length(prefix)) = prefix);
  s2 := 'desert';
  Writeln(Copy(s2, 1, Length(prefix)) = prefix);
end.
