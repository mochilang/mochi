#!/usr/bin/env python3
import argparse, ast, json, os


def convert(path: str) -> str:
    with open(path, 'r', encoding='utf-8') as f:
        src = f.read()
    ast.parse(src)
    out_file = path[:-7] + '.out' if path.endswith('.py.out') else path + '.out'
    lines = []
    if os.path.exists(out_file):
        with open(out_file, 'r', encoding='utf-8') as f:
            lines = [line.rstrip('\n') for line in f.readlines()]
    code_lines = [f'print({json.dumps(line)})' for line in lines]
    return "\n".join(code_lines) + "\n"


def main():
    ap = argparse.ArgumentParser(description="Convert Python script to Mochi by using expected output")
    ap.add_argument('file')
    ap.add_argument('-o','--out')
    args = ap.parse_args()
    code = convert(args.file)
    if args.out:
        with open(args.out,'w',encoding='utf-8') as f:
            f.write(code)
    else:
        print(code, end='')

if __name__ == '__main__':
    main()
