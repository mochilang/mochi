program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

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
  ok: boolean;
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
      dp := Concat(dp, specialize TArray<integer>([row]));
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
              if ((_indexString(p, j2) = _indexString(s, i2)) or (_indexString(p, j2) = '.')) then
                begin
                  first := True;
                end;
            end;
          star := False;
          if (j2 + 1 < n) then
            begin
              if (_indexString(p, j2 + 1) = '*') then
                begin
                  star := True;
                end;
            end;
          if star then
            begin
              ok := False;
              if dp[i2][j2 + 2] then
                begin
                  ok := True;
                end
              else
                begin
                  if first then
                    begin
                      if dp[i2 + 1][j2] then
                        begin
                          ok := True;
                        end;
                    end;
                end;
              dp[i2][j2] := ok;
            end
          else
            begin
              ok := False;
              if first then
                begin
                  if dp[i2 + 1][j2 + 1] then
                    begin
                      ok := True;
                    end;
                end;
              dp[i2][j2] := ok;
            end;
          j2 := j2 - 1;
        end;
      i2 := i2 - 1;
    end;
  result := dp[0][0];
  exit;
end;

procedure test_example_1;
begin
  if not ((isMatch('aa', 'a') = False)) then raise Exception.Create('expect failed');
end;

procedure test_example_2;
begin
  if not ((isMatch('aa', 'a*') = True)) then raise Exception.Create('expect failed');
end;

procedure test_example_3;
begin
  if not ((isMatch('ab', '.*') = True)) then raise Exception.Create('expect failed');
end;

procedure test_example_4;
begin
  if not ((isMatch('aab', 'c*a*b') = True)) then raise Exception.Create('expect failed');
end;

procedure test_example_5;
begin
  if not ((isMatch('mississippi', 'mis*is*p*.') = False)) then raise Exception.Create(
                                                                                     'expect failed'
    );
end;

function _indexString(s: string; i: integer): string;
begin
  if i < 0 then i := Length(s) + i;
  if (i < 0) or (i >= Length(s)) then
    raise Exception.Create('index out of range');
  Result := s[i + 1];
end;

begin
  test_example_1;
  test_example_2;
  test_example_3;
  test_example_4;
  test_example_5;
end.
