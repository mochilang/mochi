program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type 
  generic TArray<T> = array of T;

function addTwoNumbers(l1: specialize TArray<integer>; l2: specialize TArray<integer>): specialize
                                                                                        TArray<
                                                                                        integer>;

var 
  carry: integer;
  digit: integer;
  i: integer;
  j: integer;
  _result: specialize TArray<integer>;
  sum: function (integer): double;
  x: integer;
  y: integer;
begin
  i := 0;
  j := 0;
  carry := 0;
  _result := specialize TArray<integer>([]);
  while (((i < Length(l1)) or (j < Length(l2))) or (carry > 0)) do
    begin
      x := 0;
      if (i < Length(l1)) then
        begin
          x := specialize _indexList<integer>(l1, i);
          i := i + 1;
        end;
      y := 0;
      if (j < Length(l2)) then
        begin
          y := specialize _indexList<integer>(l2, j);
          j := j + 1;
        end;
      sum := x + y + carry;
      digit := sum mod 10;
      carry := sum div 10;
      _result := Concat(_result, specialize TArray<integer>([digit]));
    end;
  result := _result;
  exit;
end;

procedure test_example_1;
begin
  if not ((addTwoNumbers(specialize TArray<integer>([2, 4, 3]), specialize TArray<integer>([5, 6, 4]
     )) = specialize TArray<integer>([7, 0, 8]))) then raise Exception.Create('expect failed');
end;

procedure test_example_2;
begin
  if not ((addTwoNumbers(specialize TArray<integer>([0]), specialize TArray<integer>([0])) =
     specialize TArray<integer>([0]))) then raise Exception.Create('expect failed');
end;

procedure test_example_3;
begin
  if not ((addTwoNumbers(specialize TArray<integer>([9, 9, 9, 9, 9, 9, 9]), specialize TArray<
     integer>([9, 9, 9, 9])) = specialize TArray<integer>([8, 9, 9, 9, 0, 0, 0, 1]))) then raise
    Exception.Create('expect failed');
end;

generic function _indexList<T>(arr: specialize TArray<T>; i: integer): T;
begin
  if i < 0 then i := Length(arr) + i;
  if (i < 0) or (i >= Length(arr)) then
    raise Exception.Create('index out of range');
  Result := arr[i];
end;

begin
  test_example_1;
  test_example_2;
  test_example_3;
end.
