package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/charmbracelet/fang"
	"github.com/spf13/cobra"

	"mochi/tools/any2mochi"
	cconv "mochi/tools/any2mochi/x/c"
)

var (
	version   = "dev"
	gitCommit = ""
)

func parseCmd() *cobra.Command {
	var server string
	var lang string
	cmd := &cobra.Command{
		Use:   "parse [file]",
		Short: "Parse source with external CLI",
		Args:  cobra.MaximumNArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			if server == "" || lang == "" {
				return fmt.Errorf("server and lang required")
			}
			var data []byte
			var err error
			if len(args) == 1 {
				data, err = os.ReadFile(args[0])
			} else {
				data, err = io.ReadAll(cmd.InOrStdin())
			}
			if err != nil {
				return err
			}
			parts := strings.Fields(server)
			syms, diags, err := any2mochi.ParseText(parts[0], parts[1:], lang, string(data))
			if err != nil {
				return err
			}
			out := struct {
				Symbols     []any2mochi.DocumentSymbol `json:"symbols"`
				Diagnostics []any2mochi.Diagnostic     `json:"diagnostics"`
			}{syms, diags}
			enc := json.NewEncoder(cmd.OutOrStdout())
			enc.SetIndent("", "  ")
			return enc.Encode(out)
		},
	}
	cmd.Flags().StringVar(&server, "server", "", "parser CLI command")
	cmd.Flags().StringVar(&lang, "lang", "", "language id")
	return cmd
}

func convertGoCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "convert-go <file.go>",
		Short: "Convert Go source to Mochi",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			data, err := os.ReadFile(args[0])
			if err != nil {
				return err
			}
			out, err := any2mochi.ConvertGo(string(data))
			if err != nil {
				return err
			}
			_, err = cmd.OutOrStdout().Write(out)
			return err
		},
	}
	return cmd
}

func convertGoJSONCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "convert-go-json <file.go>",
		Short: "Convert Go source to Mochi via JSON AST",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			out, err := any2mochi.ConvertGoViaJSONFile(args[0])
			if err != nil {
				return err
			}
			_, err = cmd.OutOrStdout().Write(out)
			return err
		},
	}
	return cmd
}

func convertRustCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "convert-rust <file.rs>",
		Short: "Convert Rust source to Mochi",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			data, err := os.ReadFile(args[0])
			if err != nil {
				return err
			}
			out, err := any2mochi.ConvertRust(string(data))
			if err != nil {
				return err
			}
			_, err = cmd.OutOrStdout().Write(out)
			return err
		},
	}
	return cmd
}

func convertHsCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "convert-hs <file.hs>",
		Short: "Convert Haskell source to Mochi",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			data, err := os.ReadFile(args[0])
			if err != nil {
				return err
			}
			out, err := any2mochi.ConvertHs(string(data))
			if err != nil {
				return err
			}
			_, err = cmd.OutOrStdout().Write(out)
			return err
		},
	}
	return cmd
}

func convertPythonCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "convert-py <file.py>",
		Short: "Convert Python source to Mochi",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			data, err := os.ReadFile(args[0])
			if err != nil {
				return err
			}
			out, err := any2mochi.ConvertPython(string(data))
			if err != nil {
				return err
			}
			_, err = cmd.OutOrStdout().Write(out)
			return err
		},
	}
	return cmd
}

func convertTSCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "convert-ts <file.ts>",
		Short: "Convert TypeScript source to Mochi",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			data, err := os.ReadFile(args[0])
			if err != nil {
				return err
			}
			out, err := any2mochi.ConvertTypeScript(string(data))
			if err != nil {
				return err
			}
			_, err = cmd.OutOrStdout().Write(out)
			return err
		},
	}
	return cmd
}

func convertCCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "convert-c <file.c>",
		Short: "Convert C source to Mochi",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			any2mochi.UseLSP = false
			data, err := os.ReadFile(args[0])
			if err != nil {
				return err
			}
			out, err := cconv.Convert(string(data))
			if err != nil {
				return err
			}
			_, err = cmd.OutOrStdout().Write(out)
			return err
		},
	}
	return cmd
}

func convertCmd() *cobra.Command {
	var lang string
	var server string
	var ensure bool
	cmd := &cobra.Command{
		Use:   "convert [file]",
		Short: "Convert source to Mochi using a language server",
		Args:  cobra.MaximumNArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			var data []byte
			var err error
			var path string
			if len(args) == 1 {
				path = args[0]
				data, err = os.ReadFile(path)
			} else {
				data, err = io.ReadAll(cmd.InOrStdin())
			}
			if err != nil {
				return err
			}
			if lang == "" {
				lang = any2mochi.DetectLanguage(path, string(data))
			}
			var ls any2mochi.LanguageServer
			if server != "" {
				parts := strings.Fields(server)
				ls = any2mochi.LanguageServer{Command: parts[0], Args: parts[1:], LangID: lang}
			} else {
				var ok bool
				ls, ok = any2mochi.Servers[lang]
				if !ok {
					return fmt.Errorf("unknown language: %s", lang)
				}
			}
			if ensure {
				if err := any2mochi.EnsureServer(ls.Command); err != nil {
					return err
				}
			}
			syms, diags, err := any2mochi.EnsureAndParse(ls.Command, ls.Args, ls.LangID, string(data))
			if err != nil {
				return err
			}
			if len(diags) > 0 {
				return fmt.Errorf("%s", formatDiagnosticsLocal(string(data), diags))
			}
			var out strings.Builder
			for _, s := range syms {
				if s.Kind != any2mochi.SymbolKindFunction {
					continue
				}
				out.WriteString("fun ")
				out.WriteString(s.Name)
				out.WriteString("() {}\n")
			}
			if out.Len() == 0 {
				return fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippetLocal(string(data)))
			}
			_, err = cmd.OutOrStdout().Write([]byte(out.String()))
			return err
		},
	}
	cmd.Flags().StringVar(&lang, "lang", "", "language id (auto-detect if empty)")
	cmd.Flags().StringVar(&server, "server", "", "language server command")
	cmd.Flags().BoolVar(&ensure, "ensure", false, "install language server if missing")
	return cmd
}

func numberedSnippetLocal(src string) string {
	lines := strings.Split(src, "\n")
	if len(lines) > 10 {
		lines = lines[:10]
	}
	for i, l := range lines {
		lines[i] = fmt.Sprintf("%3d: %s", i+1, l)
	}
	return strings.Join(lines, "\n")
}

func formatDiagnosticsLocal(src string, diags []any2mochi.Diagnostic) string {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for _, d := range diags {
		start := int(d.Range.Start.Line)
		msg := d.Message
		line := ""
		if start < len(lines) {
			line = strings.TrimSpace(lines[start])
		}
		out.WriteString(fmt.Sprintf("line %d: %s\n  %s\n", start+1, msg, line))
	}
	return strings.TrimSpace(out.String())
}

func newRootCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "any2mochi",
		Short: "Language server utilities",
	}
	cmd.AddCommand(
		parseCmd(),
		convertGoCmd(),
		convertGoJSONCmd(),
		convertRustCmd(),
		convertPythonCmd(),
		convertHsCmd(),
		convertTSCmd(),
		convertCCmd(),
		convertCmd(),
	)
	return cmd
}

func main() {
	root := newRootCmd()
	if err := fang.Execute(
		context.Background(),
		root,
		fang.WithVersion(version),
		fang.WithCommit(gitCommit),
	); err != nil {
		os.Exit(1)
	}
}
