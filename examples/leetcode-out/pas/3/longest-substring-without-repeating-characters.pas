program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

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
          if (_indexString(s, j) = _indexString(s, i)) then
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

procedure test_example_1;
begin
  if not ((lengthOfLongestSubstring('abcabcbb') = 3)) then raise Exception.Create('expect failed');
end;

procedure test_example_2;
begin
  if not ((lengthOfLongestSubstring('bbbbb') = 1)) then raise Exception.Create('expect failed');
end;

procedure test_example_3;
begin
  if not ((lengthOfLongestSubstring('pwwkew') = 3)) then raise Exception.Create('expect failed');
end;

procedure test_empty_string;
begin
  if not ((lengthOfLongestSubstring('') = 0)) then raise Exception.Create('expect failed');
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
  test_empty_string;
end.
