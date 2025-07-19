import os, glob, pathlib
src_dir = os.path.join('tests', 'vm', 'valid')
out_dir = os.path.join('tests', 'transpiler', 'x', 'php')
files = sorted(glob.glob(os.path.join(src_dir, '*.mochi')))
lines = []
completed = 0
for f in files:
    name = pathlib.Path(f).stem
    php_file = os.path.join(out_dir, name + '.php')
    out_file = os.path.join(out_dir, name + '.out')
    err_file = os.path.join(out_dir, name + '.error')
    mark = '[ ]'
    if os.path.exists(php_file) and os.path.exists(out_file) and not os.path.exists(err_file):
        mark = '[x]'
        completed += 1
    lines.append(f'- {mark} {name}')
content = []
content.append('# PHP Transpiler Output')
content.append('')
content.append('Generated PHP code for selected Mochi programs under `tests/vm/valid`. Each program has a `.php` file and a corresponding `.out` file with the runtime output. Any execution failures are captured in a `.error` file.')
content.append('')
content.append(f'Compiled programs: {completed}/{len(files)}')
content.append('')
content.append('## Checklist')
content.extend(lines)
content.append('')
path = os.path.join('transpiler', 'x', 'php', 'README.md')
with open(path, 'w') as fh:
    fh.write('\n'.join(content))
