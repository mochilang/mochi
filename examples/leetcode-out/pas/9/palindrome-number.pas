program main;
{$mode objfpc}
uses SysUtils, fgl;

type
	generic TArray<T> = array of T;

function isPalindrome(x: integer): boolean;
var
	i: integer;
	n: integer;
	s: string;
begin
	if (x < 0) then
	begin
		result := False;
		exit;
	end;
	s := IntToStr(x);
	n := Length(s);
	for i := 0 to n div 2 - 1 do
	begin
		if (s[i + 1] <> s[n - 1 - i + 1]) then
		begin
			result := False;
			exit;
		end;
	end;
	result := True;
	exit;
end;

begin
end.
