program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

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
  book: Book;

begin
  _tmp0.title := 'Go';
  _tmp1.name := 'Bob';
  _tmp1.age := 42;
  _tmp0.author := _tmp1;
  book := _tmp0;
  writeln(book.author.name);
end.
