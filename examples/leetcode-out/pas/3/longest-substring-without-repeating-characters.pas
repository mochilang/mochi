program main;
{$mode objfpc}
uses SysUtils, fgl;

type
	generic TArray<T> = array of T;

function lengthOfLongestSubstring(s: string): integer;
var
	best: integer;
	i: integer;
	j: integer;
	_length: integer;
	n: integer;
	start: integer;
begin
	n := Length(s);
	start := 0;
	best := 0;
	i := 0;
	while (i < n) do
	begin
		j := start;
		while (j < i) do
		begin
			if (s[j] = s[i]) then
			begin
				start := j + 1;
				break;
			end;
			j := j + 1;
		end;
		_length := i - start + 1;
		if (_length > best) then
		begin
			best := _length;
		end;
		i := i + 1;
	end;
	result := best;
	exit;
end;

begin
end.
