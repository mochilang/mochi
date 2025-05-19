package repl

import (
	"fmt"
	"io"
	"strings"

	"github.com/chzyer/readline"
	"github.com/fatih/color"

	"mochi/interpreter"
	"mochi/parser"
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

	return &REPL{
		env:     env,
		interp:  interpreter.New(nil, env),
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
		printf(r.out, "%s failed to start REPL: %v\n", cError("error:"), err)
		return
	}
	defer rl.Close()
	r.rl = rl

	printWelcome(r.out, r.version)

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
				printf(r.out, "%s input cancelled\n", cNote("⏎"))
				ctrlCCount = 0
			} else {
				ctrlCCount++
				if ctrlCCount == 1 {
					printf(r.out, "%s press Ctrl+C again to exit\n", cNote("⏎"))
				} else {
					printf(r.out, "%s Exiting.\n", cNote("👋"))
					return
				}
			}
			r.setPrompt(">>> ")
			continue

		case io.EOF:
			printf(r.out, "%s Goodbye.\n", cNote("👋"))
			return

		default:
			ctrlCCount = 0
		}

		line = strings.TrimRight(line, "\r\n")

		switch cmd := strings.TrimSpace(line); cmd {
		case "":
			continue
		case ":exit":
			printf(r.out, "%s Exiting.\n", cNote("👋"))
			return
		case ":help":
			printHelp(r.out)
			continue
		}

		lines = append(lines, line)
		src := strings.Join(lines, "\n")

		prog, incomplete, err := tryParse(src)
		if err != nil {
			printf(r.out, "%s %v\n  %s\n", cError("syntax error:"), err, cHint("hint: press :help for commands"))
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
			printf(r.out, "%s %v\n  %s\n", cError("type error:"), err, cHint("hint: check types and annotations"))
			continue
		}

		r.interp.SetProgram(prog)
		if err := r.interp.Run(); err != nil {
			printf(r.out, "%s %v\n", cError("runtime error:"), err)
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

func printWelcome(out io.Writer, version string) {
	printf(out, "%s %s\n", cSuccess("🌿 Mochi Programming Language"), version)
	printf(out, "%s for help • %s to quit • %s to cancel input\n",
		cCommand(":help"),
		cCommand(":exit"),
		color.YellowString("Ctrl+C"))
	printf(out, "─────────────────────────────────────────────\n\n")
}

func printHelp(out io.Writer) {
	fmt.Fprintln(out)
	fmt.Fprintln(out, cSuccess("🌿 REPL Commands"))
	fmt.Fprintln(out, "  ----------------------------------------")
	fmt.Fprintf(out, "  %s   Show this help message\n", cCommand(":help"))
	fmt.Fprintf(out, "  %s   Exit the REPL\n", cCommand(":exit"))
}

func (r *REPL) setPrompt(base string) {
	r.rl.SetPrompt(cPrompt(base))
}

func printf(w io.Writer, format string, args ...any) {
	_, _ = fmt.Fprintf(w, format, args...)
}
