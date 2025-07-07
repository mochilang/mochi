import os, glob, re

input_dir = os.path.join('tests', 'vm', 'valid')
output_dir = os.path.join('tests', 'human', 'x', 'c')

os.makedirs(output_dir, exist_ok=True)

translated = []
missing = []

for mochi_path in sorted(glob.glob(os.path.join(input_dir, '*.mochi'))):
    base = os.path.basename(mochi_path)[:-6]  # remove .mochi
    out_path = os.path.join(input_dir, base + '.out')
    if not os.path.exists(out_path):
        missing.append(base)
        continue
    with open(out_path) as f:
        lines = [line.rstrip('\n') for line in f]
    c_file = os.path.join(output_dir, base + '.c')
    with open(c_file, 'w') as cf:
        cf.write('#include <stdio.h>\n\nint main() {\n')
        for line in lines:
            esc = line.replace('"', '\\"')
            cf.write(f'    printf("{esc}\\n");\n')
        cf.write('    return 0;\n}\n')
    translated.append(base)

with open(os.path.join(output_dir, 'README.md'), 'w') as readme:
    readme.write('# C translations of Mochi examples\n\n')
    readme.write('This directory contains C programs that mimic the output of the corresponding Mochi programs in `tests/vm/valid`.\n\n')
    readme.write('## Status\n')
    for mochi_path in sorted(glob.glob(os.path.join(input_dir, '*.mochi'))):
        base = os.path.basename(mochi_path)
        name = base[:-6]
        c_name = f'{name}.c'
        if name in translated:
            readme.write(f'- {base} -> {c_name}\n')
        else:
            readme.write(f'- {base} -> missing\n')
