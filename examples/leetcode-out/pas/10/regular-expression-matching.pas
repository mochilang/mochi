program main;
{$mode objfpc}
uses SysUtils, fgl;

type TIntArray = array of integer;

function isMatch(s: string; p: string): boolean;
var
	m: integer;
	memo: integer;
	n: integer;
begin
	m := Length(s);
	n := Length(p);
	var _tmp0: specialize TFPGMap<string, integer>;
	_tmp0 := specialize TFPGMap<string, integer>.Create;
	memo := _tmp0;
	function dfs(i: integer; j: integer): boolean;
	var
		ans: integer;
		first: integer;
		key: integer;
	begin
		key := i * n + 1 + j;
		if (key in memo) then
		begin
			result := memo[key];
			exit;
		end;
		if (j = n) then
		begin
			result := (i = m);
			exit;
		end;
		first := False;
		if (i < m) then
		begin
			if ((p[j] = s[i]) or (p[j] = '.')) then
			begin
				first := True;
			end;
		end;
		ans := False;
		if (j + 1 < n) then
		begin
			if (p[j + 1] = '*') then
			begin
				if dfs(i, j + 2) then
				begin
					ans := True;
				end else if (first and dfs(i + 1, j)) then
				begin
					ans := True;
				end;
			end else
			begin
				if (first and dfs(i + 1, j + 1)) then
				begin
					ans := True;
				end;
			end;
		end else
		begin
			if (first and dfs(i + 1, j + 1)) then
			begin
				ans := True;
			end;
		end;
		memo[key] := ans;
		result := ans;
		exit;
	end;
	result := dfs(0, 0);
	exit;
end;

begin
end.
