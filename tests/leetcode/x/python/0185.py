import sys
from collections import defaultdict


def solve_case(departments, employees):
    dept_name = {dept_id: name for dept_id, name in departments}
    groups = defaultdict(list)
    for _, name, salary, dept_id in employees:
        groups[dept_id].append((name, salary))
    out = []
    for dept_id, items in groups.items():
        keep = set(sorted({salary for _, salary in items}, reverse=True)[:3])
        for name, salary in items:
            if salary in keep:
                out.append((dept_name[dept_id], -salary, name, salary))
    out.sort()
    rows = [str(len(out))]
    rows.extend(f"{dept},{name},{salary}" for dept, _, name, salary in out)
    return "\n".join(rows)


data = sys.stdin.read().split()
if data:
    idx = 0
    t = int(data[idx])
    idx += 1
    out = []
    for _ in range(t):
        d = int(data[idx])
        e = int(data[idx + 1])
        idx += 2
        departments = []
        for _ in range(d):
            departments.append((int(data[idx]), data[idx + 1]))
            idx += 2
        employees = []
        for _ in range(e):
            employees.append((int(data[idx]), data[idx + 1], int(data[idx + 2]), int(data[idx + 3])))
            idx += 4
        out.append(solve_case(departments, employees))
    sys.stdout.write("\n\n".join(out))
