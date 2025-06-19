program main;
{$mode objfpc}
uses SysUtils, fgl;

type
	generic TArray<T> = array of T;

function convert(s: string; numRows: integer): string;
var
	ch: char;
	curr: integer;
	i: integer;
	_result: string;
	row: string;
	rows: specialize TArray<string>;
	step: integer;
begin
	if ((numRows <= 1) or (numRows >= Length(s))) then
	begin
		result := s;
		exit;
	end;
	rows := specialize TArray<string>([]);
	i := 0;
	while (i < numRows) do
	begin
		rows := Concat(rows, specialize TArray<string>(['']));
		i := i + 1;
	end;
	curr := 0;
	step := 1;
	for ch in s do
	begin
		rows[curr] := rows[curr] + ch;
		if (curr = 0) then
		begin
			step := 1;
		end else if (curr = numRows - 1) then
		begin
			step := -1;
		end;
		curr := curr + step;
	end;
	_result := '';
	for row in rows do
	begin
		_result := _result + row;
	end;
	result := _result;
	exit;
end;

begin
end.
