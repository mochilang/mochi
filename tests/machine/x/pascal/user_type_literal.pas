program UserTypeLiteral;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

type Person = record
  name: string;
  age: integer;
end;

type Book = record
  title: string;
  author: Person;
end;

var
  _tmp0: Book;
  _tmp1: Person;
  book_: Book;

begin
  _tmp0.title := 'Go';
  _tmp1.name := 'Bob';
  _tmp1.age := 42;
  _tmp0.author := _tmp1;
  book_ := _tmp0;
  writeln(book_.author.name);
end.
