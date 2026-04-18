import sys

def valid(b,r,c,ch):
    for i in range(9):
        if b[r][i] == ch or b[i][c] == ch: return False
    br, bc = (r//3)*3, (c//3)*3
    for i in range(br, br+3):
        for j in range(bc, bc+3):
            if b[i][j] == ch: return False
    return True

def solve(b):
    for r in range(9):
        for c in range(9):
            if b[r][c] == '.':
                for ch in '123456789':
                    if valid(b,r,c,ch):
                        b[r][c] = ch
                        if solve(b): return True
                        b[r][c] = '.'
                return False
    return True

def main():
    lines = sys.stdin.read().splitlines()
    if not lines: return
    idx=0; t=int(lines[idx].strip()); idx+=1; out=[]
    for tc in range(t):
        board=[list(lines[idx+i]) for i in range(9)]; idx += 9
        solve(board)
        out.extend(''.join(row) for row in board)
        if tc + 1 < t: out.append('')
    sys.stdout.write('\n'.join(out))
if __name__ == '__main__': main()
