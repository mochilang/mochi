program CastStruct;
type
  Todo = record
    title: string;
  end;
var
  todo: Todo;
begin
  todo.title := 'hi';
  Writeln(todo.title);
end.
