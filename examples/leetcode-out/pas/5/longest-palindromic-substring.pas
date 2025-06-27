program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

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
      if (_indexString(s, l) <> _indexString(s, r)) then
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
  result := _sliceString(s, start, _end + 1);
  exit;
end;

procedure test_example_1;

var 
  ans: string;
begin
  ans := longestPalindrome('babad');
  if not (((ans = 'bab') or (ans = 'aba'))) then raise Exception.Create('expect failed');
end;

procedure test_example_2;
begin
  if not ((longestPalindrome('cbbd') = 'bb')) then raise Exception.Create('expect failed');
end;

procedure test_single_char;
begin
  if not ((longestPalindrome('a') = 'a')) then raise Exception.Create('expect failed');
end;

procedure test_two_chars;

var 
  ans: string;
begin
  ans := longestPalindrome('ac');
  if not (((ans = 'a') or (ans = 'c'))) then raise Exception.Create('expect failed');
end;

function _indexString(s: string; i: integer): string;
begin
  if i < 0 then i := Length(s) + i;
  if (i < 0) or (i >= Length(s)) then
    raise Exception.Create('index out of range');
  Result := s[i + 1];
end;

function _sliceString(s: string; i, j: integer): string;

var start_, end_, n: integer;
begin
  start_ := i;
  end_ := j;
  n := Length(s);
  if start_ < 0 then start_ := n + start_;
  if end_ < 0 then end_ := n + end_;
  if start_ < 0 then start_ := 0;
  if end_ > n then end_ := n;
  if end_ < start_ then end_ := start_;
  Result := Copy(s, start_ + 1, end_ - start_);
end;

begin
  test_example_1;
  test_example_2;
  test_single_char;
  test_two_chars;
end.
