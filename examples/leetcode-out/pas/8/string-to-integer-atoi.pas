program main;
{$mode objfpc}
uses SysUtils, fgl;

type
	generic TArray<T> = array of T;

function digit(ch: string): integer;
begin
	if (ch = '0') then
	begin
		result := 0;
		exit;
	end;
	if (ch = '1') then
	begin
		result := 1;
		exit;
	end;
	if (ch = '2') then
	begin
		result := 2;
		exit;
	end;
	if (ch = '3') then
	begin
		result := 3;
		exit;
	end;
	if (ch = '4') then
	begin
		result := 4;
		exit;
	end;
	if (ch = '5') then
	begin
		result := 5;
		exit;
	end;
	if (ch = '6') then
	begin
		result := 6;
		exit;
	end;
	if (ch = '7') then
	begin
		result := 7;
		exit;
	end;
	if (ch = '8') then
	begin
		result := 8;
		exit;
	end;
	if (ch = '9') then
	begin
		result := 9;
		exit;
	end;
	result := -1;
	exit;
end;

function myAtoi(s: string): integer;
var
	ch: string;
	d: integer;
	i: integer;
	n: integer;
	_result: integer;
	sign: integer;
begin
	i := 0;
	n := Length(s);
	while ((i < n) and (s[i + 1] = ' ')) do
	begin
		i := i + 1;
	end;
	sign := 1;
	if ((i < n) and ((s[i + 1] = '+') or (s[i + 1] = '-'))) then
	begin
		if (s[i + 1] = '-') then
		begin
			sign := -1;
		end;
		i := i + 1;
	end;
	_result := 0;
	while (i < n) do
	begin
		ch := Copy(s, i + 1, (i + 1 - i));
		d := digit(ch);
		if (d < 0) then
		begin
			break;
		end;
		_result := _result * 10 + d;
		i := i + 1;
	end;
	_result := _result * sign;
	if (_result > 2147483647) then
	begin
		result := 2147483647;
		exit;
	end;
	if (_result < -2147483648) then
	begin
		result := -2147483648;
		exit;
	end;
	result := _result;
	exit;
end;

begin
end.
