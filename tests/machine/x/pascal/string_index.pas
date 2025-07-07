program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

function _indexString(s: string; i: integer): string;
begin
  if i < 0 then i := Length(s) + i;
  if (i < 0) or (i >= Length(s)) then
    raise Exception.Create('index out of range');
  Result := s[i + 1];
end;

var
  s: string;

begin
  s := 'mochi';
  writeln(_indexString(s, 1));
end.
