program ForMapCollection;

type
  Pair = record
    key: string;
    value: Integer;
  end;

const
  Count = 2;
var
  m: array[1..Count] of Pair;
  i: Integer;
begin
  m[1].key := 'a'; m[1].value := 1;
  m[2].key := 'b'; m[2].value := 2;

  for i := 1 to Count do
    Writeln(m[i].key);
end.
