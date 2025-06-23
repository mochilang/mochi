package main
import (
    "fmt"
    "mochi/runtime/jit"
)
func main(){
    expr := jit.BinOp{Op: "+", Left: jit.IntLit{Val:3}, Right: jit.BinOp{Op:"*", Left: jit.IntLit{Val:4}, Right: jit.IntLit{Val:5}}}
    fn,err := jit.Compile(expr)
    fmt.Println("err", err)
    if err==nil{
        fmt.Println("result", fn())
    }
}
