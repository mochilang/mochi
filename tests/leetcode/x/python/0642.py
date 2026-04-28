import sys


class AutocompleteSystem:
    def __init__(self, sentences: list[str], times: list[int]):
        self.counts: dict[str, int] = {}
        for s, t in zip(sentences, times):
            self.counts[s] = self.counts.get(s, 0) + t
        self.current = ""

    def input(self, c: str) -> list[str]:
        if c == "#":
            self.counts[self.current] = self.counts.get(self.current, 0) + 1
            self.current = ""
            return []
        self.current += c
        matches = [s for s in self.counts if s.startswith(self.current)]
        matches.sort(key=lambda s: (-self.counts[s], s))
        return matches[:3]


def format_list(items: list[str] | None) -> str:
    if items is None:
        return "null"
    return "[" + ",".join('"' + s + '"' for s in items) + "]"


def decode_char(token: str) -> str:
    return " " if token == "<space>" else token


def main() -> None:
    lines = [line.rstrip("\n") for line in sys.stdin.read().splitlines() if line.strip()]
    if not lines:
        return
    t = int(lines[0])
    idx = 1
    cases: list[str] = []
    for _ in range(t):
        q = int(lines[idx])
        idx += 1
        system: AutocompleteSystem | None = None
        out: list[str] = []
        for _ in range(q):
            line = lines[idx]
            idx += 1
            if line.startswith("C "):
                n = int(line.split()[1])
                sentences: list[str] = []
                times: list[int] = []
                for _ in range(n):
                    raw = lines[idx]
                    idx += 1
                    left, right = raw.split("|", 1)
                    times.append(int(left))
                    sentences.append(right)
                system = AutocompleteSystem(sentences, times)
                out.append("null")
            else:
                assert system is not None
                token = line[2:]
                out.append(format_list(system.input(decode_char(token))))
        cases.append("[" + ",".join(out) + "]")
    sys.stdout.write("\n\n".join(cases))


if __name__ == "__main__":
    main()
