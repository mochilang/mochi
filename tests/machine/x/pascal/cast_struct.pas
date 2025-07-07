program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

type Todo = record
  title: string;
end;

var
  _tmp0: specialize TFPGMap<string, Variant>;
  todo: Todo;

begin
  _tmp0 := specialize TFPGMap<string, Variant>.Create;
  _tmp0.AddOrSetData('title', 'hi');
  todo := Trunc(_tmp0);
  writeln(todo.title);
end.
