package repl

import (
	"fmt"
	"io"
	"strings"

	"github.com/chzyer/readline"

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

func (r *REPL) Run() {
	rl, err := readline.NewEx(&readline.Config{
		Prompt:          "\033[1;36m>>> \033[0m",
		HistoryLimit:    -1,
		InterruptPrompt: "^C",
		EOFPrompt:       "exit",
		Stdout:          r.out,
	})
	if err != nil {
		printf(r.out, "❌ Failed to start REPL: %v\n", err)
		return
	}
	defer func() { _ = rl.Close() }()
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
				printf(r.out, "\033[33m⏎ cancelled\033[0m\n")
				ctrlCCount = 0
			} else {
				ctrlCCount++
				if ctrlCCount == 1 {
					printf(r.out, "\033[33m⏎ cancelled (press Ctrl+C again to exit)\033[0m\n")
				} else {
					printf(r.out, "\033[33m👋 Exiting.\033[0m\n")
					return
				}
			}
			r.setPrompt(">>> ")
			continue

		case io.EOF:
			printf(r.out, "\033[33m👋 Goodbye.\033[0m\n")
			return

		default:
			ctrlCCount = 0
		}

		line = strings.TrimRight(line, "\r\n")

		switch cmd := strings.TrimSpace(line); cmd {
		case "":
			continue
		case ":exit":
			printf(r.out, "\033[33m👋 Exiting.\033[0m\n")
			return
		case ":help":
			printHelp(r.out)
			continue
		}

		lines = append(lines, line)
		src := strings.Join(lines, "\n")

		prog, incomplete, err := tryParse(src)
		if err != nil {
			printf(r.out, "\033[31mSyntax Error:\033[0m %v\n", err)
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
			printf(r.out, "\033[31mType Error:\033[0m %v\n", err)
			continue
		}

		r.interp.SetProgram(prog)
		if err := r.interp.Run(); err != nil {
			printf(r.out, "\033[31mRuntime Error:\033[0m %v\n", err)
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
	printf(out, "\033[1;32m🌿 Mochi Programming Language\033[0m\n")
	printf(out, "Version %s  •  Type \033[1;36m:help\033[0m for REPL commands\n", version)
	printf(out, "Press \033[33mCtrl+C\033[0m to cancel, \033[33mCtrl+D\033[0m or \033[1;36m:exit\033[0m to quit.\n\n")
}

func printHelp(out io.Writer) {
	printf(out, "\n  🌿 REPL Commands\n")
	printf(out, "  ---------------------------------------\n")
	printf(out, "  :help        Show this help message\n")
	printf(out, "  :exit        Exit the REPL\n")
	printf(out, "  Multiline    Use \\ or incomplete syntax to continue input\n\n")
}

func (r *REPL) setPrompt(base string) {
	r.rl.SetPrompt("\033[1;36m" + base + "\033[0m")
}

func printf(w io.Writer, format string, args ...any) {
	_, _ = fmt.Fprintf(w, format, args...)
}
