package fuzz

import (
	"encoding/binary"
	"fmt"
	"io"
	"math/rand"
	"strconv"
	"strings"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

var letters = []rune("abcdefghijklmnopqrstuvwxyz")

func randIdent(r *rand.Rand) string {
	n := r.Intn(5) + 1
	b := make([]rune, n)
	for i := range b {
		b[i] = letters[r.Intn(len(letters))]
	}
	return string(b)
}

func randExpr(r *rand.Rand, vars []string, depth int) string {
	if depth > 3 {
		if len(vars) > 0 && r.Intn(2) == 0 {
			return vars[r.Intn(len(vars))]
		}
		return strconv.Itoa(r.Intn(10))
	}

	switch r.Intn(4) {
	case 0:
		return strconv.Itoa(r.Intn(10))
	case 1:
		if len(vars) > 0 && r.Intn(2) == 0 {
			return vars[r.Intn(len(vars))]
		}
		return strconv.Itoa(r.Intn(10))
	case 2:
		return fmt.Sprintf("%s + %s", randExpr(r, vars, depth+1), randExpr(r, vars, depth+1))
	default:
		return fmt.Sprintf("(%s)", randExpr(r, vars, depth+1))
	}
}

func randStmt(r *rand.Rand, vars *[]string) string {
	switch r.Intn(2) {
	case 0:
		name := randIdent(r)
		*vars = append(*vars, name)
		return fmt.Sprintf("let %s = %s", name, randExpr(r, *vars, 0))
	default:
		return fmt.Sprintf("print(%s)", randExpr(r, *vars, 0))
	}
}

func randomProgram(r *rand.Rand) string {
	n := r.Intn(4) + 1
	vars := []string{}
	var sb strings.Builder
	for i := 0; i < n; i++ {
		if i > 0 {
			sb.WriteByte('\n')
		}
		sb.WriteString(randStmt(r, &vars))
	}
	return sb.String()
}

func FuzzVM(f *testing.F) {
	f.Add([]byte("seed"))

	f.Fuzz(func(t *testing.T, data []byte) {
		padded := make([]byte, 8)
		copy(padded, data)
		seed := int64(binary.LittleEndian.Uint64(padded))
		r := rand.New(rand.NewSource(seed))
		src := randomProgram(r)
		defer func() {
			if r := recover(); r != nil {
				t.Fatalf("panic: %v", r)
			}
		}()

		prog, err := parser.ParseString(src)
		if err != nil {
			return
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return
		}
		p, err := vm.Compile(prog, env)
		if err != nil {
			return
		}
		m := vm.New(p, io.Discard)
		_ = m.Run()
	})
}
