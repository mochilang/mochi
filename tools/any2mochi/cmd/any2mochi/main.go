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
		Short: "Parse source with a language server",
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
			syms, err := any2mochi.ParseText(parts[0], parts[1:], lang, string(data))
			if err != nil {
				return err
			}
			enc := json.NewEncoder(cmd.OutOrStdout())
			enc.SetIndent("", "  ")
			return enc.Encode(syms)
		},
	}
	cmd.Flags().StringVar(&server, "server", "", "language server command")
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

func newRootCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "any2mochi",
		Short: "Language server utilities",
	}
	cmd.AddCommand(
		parseCmd(),
		convertGoCmd(),
		convertPythonCmd(),
		convertTSCmd(),
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
