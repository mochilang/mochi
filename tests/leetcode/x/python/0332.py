import sys
from collections import defaultdict


def find_itinerary(tickets: list[tuple[str, str]]) -> list[str]:
    graph: dict[str, list[str]] = defaultdict(list)
    for src, dst in tickets:
        graph[src].append(dst)
    for dests in graph.values():
        dests.sort(reverse=True)
    route: list[str] = []

    def visit(airport: str) -> None:
        dests = graph[airport]
        while dests:
            visit(dests.pop())
        route.append(airport)

    visit("JFK")
    return route[::-1]


def fmt(route: list[str]) -> str:
    return "[" + ",".join(f'"{x}"' for x in route) + "]"


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0]); idx = 1; out: list[str] = []
    for tc in range(t):
        m = int(data[idx]); idx += 1
        tickets = []
        for _ in range(m):
            tickets.append((data[idx], data[idx + 1])); idx += 2
        if tc: out.append("")
        out.append(fmt(find_itinerary(tickets)))
    sys.stdout.write("\n".join(out))

if __name__ == "__main__":
    main()
