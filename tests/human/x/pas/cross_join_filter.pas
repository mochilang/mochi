program CrossJoinFilter;
var
  nums: array[1..3] of integer = (1,2,3);
  letters: array[1..2] of string = ('A','B');
  i,j: integer;
begin
  Writeln('--- Even pairs ---');
  for i := 1 to Length(nums) do
    if nums[i] mod 2 = 0 then
      for j := 1 to Length(letters) do
        Writeln(nums[i], ' ', letters[j]);
end.
