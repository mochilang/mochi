import sys
def _save(rows, path, opts):
    fmt = opts.get('format') if opts else 'jsonl'
    f = sys.stdout if path in ('', '-') else open(path, 'w')
    if fmt == 'jsonl':
        import json
        for r in rows:
            print(json.dumps(r), file=f)

people = [{"name": "Alice", "age": 30}, {"name": "Bob", "age": 25}]
_save(people, "-", {"format": "jsonl"})
