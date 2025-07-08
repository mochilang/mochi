class Todo {
  String title;
  Todo(this.title);
}

void main() {
  var todo = Todo(({'title': 'hi'}['title'] as String));
  print(todo.title);
}
