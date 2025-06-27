program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type 
  generic TArray<T> = array of T;

function isPalindrome(x: integer): boolean;

var 
  i: integer;
  n: integer;
  s: string;
begin
  if (x < 0) then
    begin
      result := False;
      exit;
    end;
  s := IntToStr(x);
  n := Length(s);
  for i := 0 to n div 2 - 1 do
    begin
      if (s[i] <> s[n - 1 - i]) then
        begin
          result := False;
          exit;
        end;
    end;
  result := True;
  exit;
end;

procedure test_example_1;
begin
  if not ((isPalindrome(121) = True)) then raise Exception.Create('expect failed');
end;

procedure test_example_2;
begin
  if not ((isPalindrome(-121) = False)) then raise Exception.Create('expect failed');
end;

procedure test_example_3;
begin
  if not ((isPalindrome(10) = False)) then raise Exception.Create('expect failed');
end;

procedure test_zero;
begin
  if not ((isPalindrome(0) = True)) then raise Exception.Create('expect failed');
end;

begin
  test_example_1;
  test_example_2;
  test_example_3;
  test_zero;
end.
