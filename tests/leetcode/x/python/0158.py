import sys


def quote(s):
    return '"' + s + '"'


lines = sys.stdin.read().splitlines()
if lines:
    tc = int(lines[0])
    idx = 1
    out = []
    for _ in range(tc):
        data = lines[idx]
        idx += 1
        q = int(lines[idx])
        idx += 1
        pos = 0
        block = [str(q)]
        for _ in range(q):
            n = int(lines[idx])
            idx += 1
            part = data[pos:pos + n]
            pos += len(part)
            block.append(quote(part))
        out.append("\n".join(block))
    sys.stdout.write("\n\n".join(out))
