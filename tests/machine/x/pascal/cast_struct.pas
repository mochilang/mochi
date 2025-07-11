program CastStruct;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

type Todo = record
  title: string;
end;

var
  _tmp0: Todo;
  todo_: Todo;

begin
  _tmp0.title := 'hi';
  todo_ := _tmp0;
  writeln(todo_.title);
end.
