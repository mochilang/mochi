program UserTypeLiteral;

type
  Person = record
    name: string;
    age: integer;
  end;

  Book = record
    title: string;
    author: Person;
  end;

var
  book: Book;
begin
  book.title := 'Go';
  book.author.name := 'Bob';
  book.author.age := 42;
  Writeln(book.author.name);
end.
