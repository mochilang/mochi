program main;
{$mode objfpc}
uses SysUtils, fgl;

type
	generic TArray<T> = array of T;

function addTwoNumbers(l1: specialize TArray<integer>; l2: specialize TArray<integer>): specialize TArray<integer>;
var
	carry: integer;
	digit: integer;
	i: integer;
	j: integer;
	_result: specialize TArray<integer>;
	sum: integer;
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
		_result := Concat(_result, specialize TArray<integer>([digit]));
	end;
	result := _result;
	exit;
end;

begin
end.
