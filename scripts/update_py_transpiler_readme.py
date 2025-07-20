import os, glob, pathlib

src_dir = os.path.join('tests', 'vm', 'valid')
out_dir = os.path.join('tests', 'transpiler', 'x', 'py')
files = sorted(glob.glob(os.path.join(src_dir, '*.mochi')))
completed = 0
lines = []
for f in files:
    name = pathlib.Path(f).stem
    out_file = os.path.join(out_dir, name + '.out')
    err_file = os.path.join(out_dir, name + '.error')
    mark = '[ ]'
    if os.path.exists(out_file) and not os.path.exists(err_file):
        mark = '[x]'
        completed += 1
    lines.append(f'- {mark} {name}')
content = []
content.append('# Transpiler Progress')
content.append('')
content.append('This checklist is auto-generated.')
content.append('Generated Python code from programs in `tests/vm/valid` lives in `tests/transpiler/x/py`.')
content.append('')
content.append(f'## VM Golden Test Checklist ({completed}/{len(files)})')
content.extend(lines)
content.append('')
path = os.path.join('transpiler', 'x', 'py', 'README.md')
with open(path, 'w') as fh:
    fh.write('\n'.join(content))
