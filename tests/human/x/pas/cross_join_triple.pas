program CrossJoinTriple;
var
  nums: array[1..2] of integer = (1,2);
  letters: array[1..2] of string = ('A','B');
  bools: array[1..2] of boolean = (True, False);
  i,j,k: integer;
  bstr: string;
begin
  Writeln('--- Cross Join of three lists ---');
  for i := 1 to Length(nums) do
    for j := 1 to Length(letters) do
      for k := 1 to Length(bools) do
      begin
        if bools[k] then bstr := 'true' else bstr := 'false';
        Writeln(nums[i], ' ', letters[j], ' ', bstr);
      end;
end.
