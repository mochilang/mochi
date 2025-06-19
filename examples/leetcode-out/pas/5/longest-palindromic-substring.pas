program main;
{$mode objfpc}
uses SysUtils, fgl;

type
	generic TArray<T> = array of T;

function expand(s: string; left: integer; right: integer): integer;
var
	l: integer;
	n: integer;
	r: integer;
begin
	l := left;
	r := right;
	n := Length(s);
	while ((l >= 0) and (r < n)) do
	begin
		if (s[l + 1] <> s[r + 1]) then
		begin
			break;
		end;
		l := l - 1;
		r := r + 1;
	end;
	result := r - l - 1;
	exit;
end;

function longestPalindrome(s: string): string;
var
	_end: integer;
	i: integer;
	l: integer;
	len1: integer;
	len2: integer;
	n: integer;
	start: integer;
begin
	if (Length(s) <= 1) then
	begin
		result := s;
		exit;
	end;
	start := 0;
	_end := 0;
	n := Length(s);
	for i := 0 to n - 1 do
	begin
		len1 := expand(s, i, i);
		len2 := expand(s, i, i + 1);
		l := len1;
		if (len2 > len1) then
		begin
			l := len2;
		end;
		if (l > _end - start) then
		begin
			start := i - l - 1 div 2;
			_end := i + l div 2;
		end;
	end;
	result := Copy(s, start + 1, (_end + 1 - start));
	exit;
end;

begin
end.
