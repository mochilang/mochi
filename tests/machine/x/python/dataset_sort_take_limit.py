from __future__ import annotations
from typing import Any, TypeVar, Generic, Callable

T = TypeVar("T")
K = TypeVar("K")


def _sort_key(k):
    if isinstance(k, (list, tuple, dict)):
        return str(k)
    return k


products = [
    {"name": "Laptop", "price": 1500},
    {"name": "Smartphone", "price": 900},
    {"name": "Tablet", "price": 600},
    {"name": "Monitor", "price": 300},
    {"name": "Keyboard", "price": 100},
    {"name": "Mouse", "price": 50},
    {"name": "Headphones", "price": 200},
]
expensive = [
    p
    for p in sorted([p for p in products], key=lambda p: _sort_key(-p["price"]))[
        max(1, 0) :
    ][: max(3, 0)]
]
print("--- Top products (excluding most expensive) ---")
for item in expensive:
    print(item["name"], "costs $", item["price"])
