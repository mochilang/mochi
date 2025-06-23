package repl

import (
	"fmt"
	"strings"

	"github.com/charmbracelet/bubbles/help"
	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/textarea"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/fatih/color"

	"mochi/types"
)

type keyMap struct {
	Eval  key.Binding
	Exit  key.Binding
	Help  key.Binding
	Clear key.Binding
}

func (k keyMap) ShortHelp() []key.Binding {
	return []key.Binding{k.Eval, k.Exit, k.Help, k.Clear}
}

func (k keyMap) FullHelp() [][]key.Binding { return [][]key.Binding{k.ShortHelp()} }

var keys = keyMap{
	Eval:  key.NewBinding(key.WithKeys("enter", "ctrl+e"), key.WithHelp("enter", "eval")),
	Exit:  key.NewBinding(key.WithKeys("ctrl+c"), key.WithHelp("ctrl+c", "exit")),
	Help:  key.NewBinding(key.WithKeys("ctrl+h"), key.WithHelp("ctrl+h", "help")),
	Clear: key.NewBinding(key.WithKeys("ctrl+l"), key.WithHelp("ctrl+l", "clear")),
}

// teaModel implements a Bubble Tea TUI for the Mochi REPL.
type teaModel struct {
	repl     *REPL
	input    textarea.Model
	help     help.Model
	output   []string
	lines    []string
	prompt   string
	showHelp bool
}

func newTeaModel(r *REPL) teaModel {
	ta := textarea.New()
	ta.Focus()
	ta.Prompt = cPrompt(">>> ")
	ta.CharLimit = 0
	ta.ShowLineNumbers = false
	ta.SetWidth(80)

	h := help.New()
	h.ShowAll = false

	return teaModel{
		repl:   r,
		input:  ta,
		help:   h,
		prompt: ">>> ",
	}
}

func (m teaModel) Init() tea.Cmd { return textarea.Blink }

func (m teaModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch {
		case key.Matches(msg, keys.Exit):
			if len(m.lines) > 0 {
				m.lines = nil
				m.appendOutput(fmt.Sprintf("%s input cancelled", cNote("⏎")))
				m.setPrompt(">>> ")
				m.input.Reset()
				return m, nil
			}
			m.appendOutput(fmt.Sprintf("%s Exiting.", cNote("👋")))
			return m, tea.Quit
		case key.Matches(msg, keys.Clear):
			m.output = nil
			return m, nil
		case key.Matches(msg, keys.Help):
			m.showHelp = !m.showHelp
			return m, nil
		case key.Matches(msg, keys.Eval):
			line := m.input.Value()
			m.input.Reset()
			switch cmd := strings.TrimSpace(line); cmd {
			case "":
				return m, nil
			case ":exit":
				m.appendOutput(fmt.Sprintf("%s Exiting.", cNote("👋")))
				return m, tea.Quit
			case ":help":
				m.appendOutput(helpString())
				return m, nil
			}
			m.lines = append(m.lines, line)
			src := strings.Join(m.lines, "\n")
			prog, incomplete, err := tryParse(src)
			if err != nil {
				m.appendOutput(fmt.Sprintf("%s %v\n  %s", cError("syntax error:"), err, cHint("hint: press :help for commands")))
				m.lines = nil
				m.setPrompt(">>> ")
				return m, nil
			}
			if incomplete {
				m.setPrompt("... ")
				return m, nil
			}
			m.lines = nil
			m.setPrompt(">>> ")
			if err := types.Check(prog, m.repl.env); err != nil {
				m.appendOutput(fmt.Sprintf("%s %v\n  %s", cError("type error:"), err, cHint("hint: check types and annotations")))
				return m, nil
			}
			m.repl.interp.SetProgram(prog)
			if err := m.repl.interp.Run(); err != nil {
				m.appendOutput(fmt.Sprintf("%s %v", cError("runtime error:"), err))
			}
			return m, nil
		}
	}
	var cmd tea.Cmd
	m.input, cmd = m.input.Update(msg)
	return m, cmd
}

func (m teaModel) View() string {
	out := strings.Join(m.output, "\n")
	if out != "" {
		out += "\n"
	}
	m.input.Prompt = cPrompt(m.prompt)
	view := out + m.input.View()
	if m.showHelp {
		view += "\n\n" + m.help.View(keys)
	}
	return view
}

func (m *teaModel) setPrompt(p string) {
	m.prompt = p
}

func (m *teaModel) appendOutput(s string) {
	m.output = append(m.output, s)
}

func helpString() string {
	return fmt.Sprintf(
		"\n%s\n  -------------------------------\n  %s   Show this help message\n  %s   Exit the REPL\n  %s   Clear output\n  %s   Evaluate input\n",
		cSuccess("🌿 REPL Commands"),
		cCommand(":help"),
		cCommand(":exit"),
		cCommand("Ctrl+L"),
		cCommand("Enter/Ctrl+E"),
	)
}

func welcomeString(version string) string {
	return fmt.Sprintf("%s %s\n%s for help • %s to quit • %s to cancel input\n─────────────────────────────────────────────\n", cSuccess("🌿 Mochi Programming Language"), version, cCommand(":help"), cCommand(":exit"), color.YellowString("Ctrl+C"))
}

func (r *REPL) RunTUI() {
	m := newTeaModel(r)
	m.appendOutput(welcomeString(r.version))
        p := tea.NewProgram(m, tea.WithAltScreen())
	if err := p.Start(); err != nil {
		printf(r.out, "%s failed to start TUI: %v\n", cError("error:"), err)
	}
}
