program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type 
  generic TArray<T> = array of T;

function reverse(x: integer): integer;

var 
  digit: integer;
  n: integer;
  rev: integer;
  sign: integer;
begin
  sign := 1;
  n := x;
  if (n < 0) then
    begin
      sign := -1;
      n := -n;
    end;
  rev := 0;
  while (n <> 0) do
    begin
      digit := n mod 10;
      rev := rev * 10 + digit;
      n := n div 10;
    end;
  rev := rev * sign;
  if ((rev < -2147483647 - 1) or (rev > 2147483647)) then
    begin
      result := 0;
      exit;
    end;
  result := rev;
  exit;
end;

procedure test_example_1;
begin
  if not ((reverse(123) = 321)) then raise Exception.Create('expect failed');
end;

procedure test_example_2;
begin
  if not ((reverse(-123) = -321)) then raise Exception.Create('expect failed');
end;

procedure test_example_3;
begin
  if not ((reverse(120) = 21)) then raise Exception.Create('expect failed');
end;

procedure test_overflow;
begin
  if not ((reverse(1534236469) = 0)) then raise Exception.Create('expect failed');
end;

begin
  test_example_1;
  test_example_2;
  test_example_3;
  test_overflow;
end.
