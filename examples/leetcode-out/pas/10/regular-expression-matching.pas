program main;
{$mode objfpc}
uses SysUtils, fgl;

type
	generic TArray<T> = array of T;

function isMatch(s: string; p: string): boolean;
var
	dp: specialize TArray<specialize TArray<boolean>>;
	first: boolean;
	i: integer;
	i2: integer;
	j: integer;
	j2: integer;
	m: integer;
	n: integer;
	row: specialize TArray<boolean>;
	star: boolean;
begin
	m := Length(s);
	n := Length(p);
	dp := specialize TArray<specialize TArray<boolean>>([]);
	i := 0;
	while (i <= m) do
	begin
		row := specialize TArray<boolean>([]);
		j := 0;
		while (j <= n) do
		begin
			row := Concat(row, specialize TArray<boolean>([False]));
			j := j + 1;
		end;
		dp := Concat(dp, specialize TArray<specialize TArray<boolean>>([row]));
		i := i + 1;
	end;
	dp[m][n] := True;
	i2 := m;
	while (i2 >= 0) do
	begin
		j2 := n - 1;
		while (j2 >= 0) do
		begin
			first := False;
			if (i2 < m) then
			begin
				if ((p[j2 + 1] = s[i2 + 1]) or (p[j2 + 1] = '.')) then
				begin
					first := True;
				end;
			end;
			star := False;
			if (j2 + 1 < n) then
			begin
				if (p[j2 + 1 + 1] = '*') then
				begin
					star := True;
				end;
			end;
			if star then
			begin
				if (dp[i2][j2 + 2] or (first and dp[i2 + 1][j2])) then
				begin
					dp[i2][j2] := True;
				end else
				begin
					dp[i2][j2] := False;
				end;
			end else
			begin
				if (first and dp[i2 + 1][j2 + 1]) then
				begin
					dp[i2][j2] := True;
				end else
				begin
					dp[i2][j2] := False;
				end;
			end;
			j2 := j2 - 1;
		end;
		i2 := i2 - 1;
	end;
	result := dp[0][0];
	exit;
end;

begin
end.
