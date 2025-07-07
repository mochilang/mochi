program Membership;

function Contains(arr: array of integer; v: integer): boolean;
var
  i: integer;
begin
  for i := Low(arr) to High(arr) do
    if arr[i] = v then
    begin
      Contains := True;
      Exit;
    end;
  Contains := False;
end;

var
  nums: array[0..2] of integer = (1,2,3);
begin
  Writeln(Contains(nums, 2));
  Writeln(Contains(nums, 4));
end.
