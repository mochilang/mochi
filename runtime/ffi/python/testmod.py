"""Test module for Infer"""

__all__ = ["add", "answer", "PI", "Point"]

PI: float = 3.14
answer: int = 42


def add(a: int, b: int) -> int:
    """Add sums a and b."""
    return a + b


class Point:
    """Point is a simple class used for testing."""

    x: int = 0
    y: int = 0

    def __init__(self, x: int, y: int):
        self.x = x
        self.y = y

    def magnitude(self) -> float:
        """Return the Euclidean norm."""
        return (self.x ** 2 + self.y ** 2) ** 0.5
