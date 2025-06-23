package repl

import (
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/chzyer/readline"
	"github.com/fatih/color"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/runtime/mod"
	"mochi/types"
)

type REPL struct {
	env     *types.Env
	interp  *interpreter.Interpreter
	rl      *readline.Instance
	out     io.Writer
	version string
}

func New(out io.Writer, version string) *REPL {
	env := types.NewEnv(nil)
	env.SetWriter(out)

	root, err := mod.FindRoot(".")
	if err != nil {
		root, _ = os.Getwd()
	}

	return &REPL{
		env:     env,
		interp:  interpreter.New(nil, env, root),
		out:     out,
		version: version,
	}
}

// Color helpers for DX consistency
var (
	cPrompt  = color.New(color.FgCyan, color.Bold).SprintFunc()
	cError   = color.New(color.FgRed, color.Bold).SprintFunc()
	cNote    = color.New(color.FgBlue).SprintFunc()
	cHint    = color.New(color.FgHiBlack).SprintFunc()
	cSuccess = color.New(color.FgGreen, color.Bold).SprintFunc()
	cCommand = color.New(color.FgCyan).SprintFunc()
)

func (r *REPL) Run() {
	rl, err := readline.NewEx(&readline.Config{
		Prompt:          cPrompt(">>> "),
		HistoryLimit:    -1,
		InterruptPrompt: "^C",
		EOFPrompt:       "exit",
		Stdout:          r.out,
	})
	if err != nil {
		r.printf("%s failed to start REPL: %v\n", cError("error:"), err)
		return
	}
	defer func() {
		if err := rl.Close(); err != nil {
			r.printf("%s failed to close readline: %v\n", cError("error:"), err)
		}
	}()
	r.rl = rl

	r.printWelcome()

	var (
		lines      []string
		ctrlCCount int
	)

	for {
		line, err := rl.Readline()

		switch err {
		case readline.ErrInterrupt:
			if len(lines) > 0 {
				lines = nil
				r.printf("%s input cancelled\n", cNote("⏎"))
				ctrlCCount = 0
			} else {
				ctrlCCount++
				if ctrlCCount == 1 {
					r.printf("%s press Ctrl+C again to exit\n", cNote("⏎"))
				} else {
					r.printf("%s Exiting.\n", cNote("👋"))
					return
				}
			}
			r.setPrompt(">>> ")
			continue

		case io.EOF:
			r.printf("%s Goodbye.\n", cNote("👋"))
			return

		default:
			ctrlCCount = 0
		}

		line = strings.TrimRight(line, "\r\n")

		switch cmd := strings.TrimSpace(line); cmd {
		case "":
			continue
		case ":exit":
			r.printf("%s Exiting.\n", cNote("👋"))
			return
		case ":help":
			r.printHelp()
			continue
		}

		lines = append(lines, line)
		src := strings.Join(lines, "\n")

		prog, incomplete, err := tryParse(src)
		if err != nil {
			r.printf("%s %v\n  %s\n", cError("syntax error:"), err, cHint("hint: press :help for commands"))
			lines = nil
			r.setPrompt(">>> ")
			continue
		}
		if incomplete {
			r.setPrompt("... ")
			continue
		}

		lines = nil
		r.setPrompt(">>> ")

		if err := types.Check(prog, r.env); err != nil {
			r.printf("%s %v\n  %s\n", cError("type error:"), err, cHint("hint: check types and annotations"))
			continue
		}

		r.interp.SetProgram(prog)
		result, err := r.interp.RunResult()
		if err != nil {
			r.printf("%s %v\n", cError("runtime error:"), err)
			continue
		}
		if result != nil {
			r.printf("%v\n", result)
		}
	}
}

func tryParse(src string) (*parser.Program, bool, error) {
	prog, err := parser.Parser.ParseString("<repl>", src)
	if err == nil {
		return prog, false, nil
	}
	if strings.Contains(err.Error(), `unexpected token "<EOF>"`) {
		return nil, true, nil
	}
	return nil, false, err
}

func (r *REPL) printWelcome() {
	r.printf("%s %s\n", cSuccess("🌿 Mochi Programming Language"), r.version)
	r.printf("%s for help • %s to quit • %s to cancel input\n",
		cCommand(":help"),
		cCommand(":exit"),
		color.YellowString("Ctrl+C"))
	r.printf("─────────────────────────────────────────────\n\n")
}

func (r *REPL) printHelp() {
	out := r.rl.Stdout()
	if _, err := fmt.Fprintln(out); err != nil {
		return
	}
	if _, err := fmt.Fprintln(out, cSuccess("🌿 REPL Commands")); err != nil {
		return
	}
	if _, err := fmt.Fprintln(out, "  ----------------------------------------"); err != nil {
		return
	}
	if _, err := fmt.Fprintf(out, "  %s   Show this help message\n", cCommand(":help")); err != nil {
		return
	}
	if _, err := fmt.Fprintf(out, "  %s   Exit the REPL\n", cCommand(":exit")); err != nil {
		return
	}
}

func (r *REPL) setPrompt(base string) {
	r.rl.SetPrompt(cPrompt(base))
}

func (r *REPL) printf(format string, args ...any) {
	if r != nil && r.rl != nil {
		_, _ = fmt.Fprintf(r.rl.Stdout(), format, args...)
		return
	}
	_, _ = fmt.Fprintf(r.out, format, args...)
}

// printf is kept for compatibility with existing functions that accept an
// io.Writer. It simply writes to the provided writer.
func printf(w io.Writer, format string, args ...any) {
	_, _ = fmt.Fprintf(w, format, args...)
}
