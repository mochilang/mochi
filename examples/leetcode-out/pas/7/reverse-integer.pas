program main;
{$mode objfpc}
uses SysUtils, fgl;

type TIntArray = array of integer;

function reverse(x: integer): integer;
var
	digit: integer;
	n: integer;
	rev: integer;
	sign: integer;
begin
	sign := 1;
	n := x;
	if (n < 0) then
	begin
		sign := -1;
		n := -n;
	end;
	rev := 0;
	while (n <> 0) do
	begin
		digit := n mod 10;
		rev := rev * 10 + digit;
		n := n div 10;
	end;
	rev := rev * sign;
	if ((rev < -2147483647 - 1) or (rev > 2147483647)) then
	begin
		result := 0;
		exit;
	end;
	result := rev;
	exit;
end;

begin
end.
