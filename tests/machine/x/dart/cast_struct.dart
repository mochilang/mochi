class Todo {
  String title;
  Todo(this.title);
}

var todo = Todo(({'title': 'hi'}['title'] as String));

void main() {
  print(todo.title);
}
