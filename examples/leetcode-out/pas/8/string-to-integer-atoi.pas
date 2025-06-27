program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

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
  while ((i < n) and (_indexString(s, i) = _indexString(' ', 0))) do
    begin
      i := i + 1;
    end;
  sign := 1;
  if ((i < n) and ((_indexString(s, i) = _indexString('+', 0)) or (_indexString(s, i) = _indexString
     ('-', 0)))) then
    begin
      if (_indexString(s, i) = _indexString('-', 0)) then
        begin
          sign := -1;
        end;
      i := i + 1;
    end;
  _result := 0;
  while (i < n) do
    begin
      ch := _sliceString(s, i, i + 1);
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

procedure test_example_1;
begin
  if not ((myAtoi('42') = 42)) then raise Exception.Create('expect failed');
end;

procedure test_example_2;
begin
  if not ((myAtoi('   -42') = -42)) then raise Exception.Create('expect failed');
end;

procedure test_example_3;
begin
  if not ((myAtoi('4193 with words') = 4193)) then raise Exception.Create('expect failed');
end;

procedure test_example_4;
begin
  if not ((myAtoi('words and 987') = 0)) then raise Exception.Create('expect failed');
end;

procedure test_example_5;
begin
  if not ((myAtoi('-91283472332') = -2147483648)) then raise Exception.Create('expect failed');
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
  test_example_3;
  test_example_4;
  test_example_5;
end.
