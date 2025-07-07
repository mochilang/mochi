from dataclasses import dataclass

@dataclass
class Todo:
    title: str

todo = Todo(title="hi")
print(todo.title)
