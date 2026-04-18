package main
import "fmt"
func valid(b *[9][9]byte, r,c int, ch byte) bool { for i:=0;i<9;i++ { if b[r][i]==ch || b[i][c]==ch { return false } }; br,bc:=(r/3)*3,(c/3)*3; for i:=br;i<br+3;i++ { for j:=bc;j<bc+3;j++ { if b[i][j]==ch { return false } } }; return true }
func solve(b *[9][9]byte) bool { for r:=0;r<9;r++ { for c:=0;c<9;c++ { if b[r][c]=='.' { for ch:=byte('1'); ch<=byte('9'); ch++ { if valid(b,r,c,ch) { b[r][c]=ch; if solve(b) { return true }; b[r][c]='.' } }; return false } } }; return true }
func main(){ var t int; if _,err:=fmt.Scan(&t); err!=nil { return }; for tc:=0; tc<t; tc++ { var b [9][9]byte; for i:=0;i<9;i++ { var s string; fmt.Scan(&s); copy(b[i][:], []byte(s)) }; solve(&b); for i:=0;i<9;i++ { fmt.Print(string(b[i][:])); if tc+1<t || i<8 { fmt.Print("\n") } } } }
