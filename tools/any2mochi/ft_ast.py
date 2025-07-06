import sys, json, re

src = sys.stdin.read().splitlines()
funcs = []
i = 0
while i < len(src):
    line = src[i].strip()
    m = re.match(r"(program|function|subroutine)\s+(\w+)(?:\(([^)]*)\))?", line, re.IGNORECASE)
    if m:
        kind, name, params = m.groups()
        params = [p.strip() for p in params.split(',')] if params else []
        body = []
        i += 1
        while i < len(src):
            l = src[i].strip()
            if re.match(r"end\s+(program|function|subroutine)\s+"+re.escape(name), l, re.IGNORECASE):
                break
            if l.lower().startswith('print'):
                parts = l.split(',',1)
                expr = parts[1].strip() if len(parts) > 1 else ''
                body.append({'kind':'print','expr':expr})
            elif '=' in l:
                left,right = l.split('=',1)
                body.append({'kind':'assign','var':left.strip(),'expr':right.strip()})
            i += 1
        funcs.append({'name':name,'params':params,'body':body})
    i += 1
json.dump({'funcs':funcs}, sys.stdout)
