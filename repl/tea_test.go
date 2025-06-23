package repl

import (
	"bytes"
	"testing"

	tea "github.com/charmbracelet/bubbletea"
)

func TestTeaModelEvalExpr(t *testing.T) {
	out := &bytes.Buffer{}
	r := New(out, "")
	m := newTeaModel(r)
	m.input.SetValue("1 + 4")
	next, _ := m.Update(tea.KeyMsg{Type: tea.KeyEnter})
	m = next.(teaModel)
	if len(m.output) == 0 || m.output[len(m.output)-1] != "5" {
		t.Fatalf("expected output 5, got %v", m.output)
	}
}
