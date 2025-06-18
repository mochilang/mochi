program main;
{$mode objfpc}
uses SysUtils, fgl;

type TIntArray = array of integer;

function myAtoi(s: string): integer;
var
	ch: integer;
	d: integer;
	digits: integer;
	i: integer;
	n: integer;
	_result: integer;
	sign: integer;
begin
	i := 0;
	n := Length(s);
	while ((i < n) and (s[i] = ' ')) do
	begin
		i := i + 1;
	end;
	sign := 1;
	if ((i < n) and ((s[i] = '+') or (s[i] = '-'))) then
	begin
		if (s[i] = '-') then
		begin
			sign := -1;
		end;
		i := i + 1;
	end;
	var _tmp0: specialize TFPGMap<string, integer>;
	_tmp0 := specialize TFPGMap<string, integer>.Create;
	_tmp0.AddOrSetData('0', 0);
	_tmp0.AddOrSetData('1', 1);
	_tmp0.AddOrSetData('2', 2);
	_tmp0.AddOrSetData('3', 3);
	_tmp0.AddOrSetData('4', 4);
	_tmp0.AddOrSetData('5', 5);
	_tmp0.AddOrSetData('6', 6);
	_tmp0.AddOrSetData('7', 7);
	_tmp0.AddOrSetData('8', 8);
	_tmp0.AddOrSetData('9', 9);
	digits := _tmp0;
	_result := 0;
	while (i < n) do
	begin
		ch := s[i];
		if not (ch in digits) then
		begin
			break;
		end;
		d := digits[ch];
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
