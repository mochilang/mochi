program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

  generic function _sliceList<T>(arr: specialize TArray<T>; i, j: integer): specialize TArray<T>;

var start_, end_, n: integer;
begin
  start_ := i;
  end_ := j;
  n := Length(arr);
  if start_ < 0 then start_ := n + start_;
  if end_ < 0 then end_ := n + end_;
  if start_ < 0 then start_ := 0;
  if end_ > n then end_ := n;
  if end_ < start_ then end_ := start_;
  Result := Copy(arr, start_ + 1, end_ - start_);
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
  writeln(specialize _sliceList<integer>(specialize TArray<integer>([1, 2, 3]), 1, 3));
  writeln(specialize _sliceList<integer>(specialize TArray<integer>([1, 2, 3]), 0, 2));
  writeln(_sliceString('hello', 1, 4));
end.
