program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  s: string;

begin
  s := 'catch';
  writeln(s.contains('cat'));
  writeln(s.contains('dog'));
end.
