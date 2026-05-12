package vm

import (
	"fmt"
	"strings"
)

type VMError struct {
	Err   error
	Stack []StackFrame
}

func (e *VMError) Error() string {
	return e.Err.Error()
}

// Format returns a detailed representation of the VM error including the call
// graph and annotated stack trace when program information is available.
func (e *VMError) Format(prog *Program) string {
	var b strings.Builder
	b.WriteString(e.Err.Error())
	if prog != nil {
		b.WriteString("\ncall graph: ")
		b.WriteString(e.callGraph())
		b.WriteString("\nstack trace:\n")
		b.WriteString(e.stackTrace(prog))
	}
	return b.String()
}

func (e *VMError) callGraph() string {
	names := make([]string, len(e.Stack))
	for i, f := range e.Stack {
		names[i] = f.Func
	}
	return strings.Join(names, " -> ")
}

func (e *VMError) stackTrace(prog *Program) string {
	var b strings.Builder
	for i := len(e.Stack) - 1; i >= 0; i-- {
		f := e.Stack[i]
		lineInfo := ""
		if prog != nil && f.Line > 0 && f.Line <= len(prog.Source) {
			lineInfo = strings.TrimSpace(prog.Source[f.Line-1])
		}
		if lineInfo != "" {
			fmt.Fprintf(&b, "  %s at %s:%d\n    %s\n", f.Func, prog.File, f.Line, lineInfo)
		} else {
			fmt.Fprintf(&b, "  %s:%d\n", f.Func, f.Line)
		}
	}
	return b.String()
}

func copyTrace(trace []StackFrame) []StackFrame {
	out := make([]StackFrame, len(trace))
	copy(out, trace)
	return out
}

func (m *VM) newError(err error, trace []StackFrame, line int) *VMError {
	tr := copyTrace(trace)
	if len(tr) > 0 {
		tr[len(tr)-1].Line = line
	}
	if vmErr, ok := err.(*VMError); ok {
		vmErr.Stack = tr
		return vmErr
	}
	return &VMError{Err: err, Stack: tr}
}

