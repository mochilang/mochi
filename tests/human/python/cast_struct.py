# Translation of cast_struct.mochi to Python

from dataclasses import dataclass

@dataclass
class Todo:
    title: str

todo = Todo(title="hi")
print(todo.title)
