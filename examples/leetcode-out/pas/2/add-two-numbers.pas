program main;
{$mode objfpc}
uses SysUtils;

type TIntArray = array of integer;

function addTwoNumbers(l1: TIntArray; l2: TIntArray): TIntArray;
var
	carry: integer;
	digit: integer;
	i: integer;
	j: integer;
	_result: TIntArray;
	sum: integer;
	x: integer;
	y: integer;
begin
	i := 0;
	j := 0;
	carry := 0;
	_result := TIntArray([]);
	while (((i < Length(l1)) or (j < Length(l2))) or (carry > 0)) do
	begin
		x := 0;
		if (i < Length(l1)) then
		begin
			x := l1[i];
			i := i + 1;
		end;
		y := 0;
		if (j < Length(l2)) then
		begin
			y := l2[j];
			j := j + 1;
		end;
		sum := x + y + carry;
		digit := sum mod 10;
		carry := sum div 10;
		_result := Concat(_result, TIntArray([digit]));
	end;
	result := _result;
	exit;
end;

begin
end.
