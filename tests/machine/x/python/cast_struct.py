from __future__ import annotations
import dataclasses


@dataclasses.dataclass
class Todo:
    title: str


todo = Todo(**{"title": "hi"})
print(todo.title)
