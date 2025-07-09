program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

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

var
  prefix: string;
  s1: string;
  s2: string;

begin
  prefix := 'fore';
  s1 := 'forest';
  writeln((_sliceString(s1, 0, Length(prefix)) = prefix));
  s2 := 'desert';
  writeln((_sliceString(s2, 0, Length(prefix)) = prefix));
end.
