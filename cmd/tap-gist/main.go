package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"

	"github.com/charmbracelet/fang"
	"github.com/spf13/cobra"

	"mochi/tools/tap/github"
)

var (
	version   = "dev"
	gitCommit = ""
)

type rootFlags struct {
	Message string
	Public  bool
	Token   string
}

func newRootCmd() *cobra.Command {
	var flags rootFlags
	cmd := &cobra.Command{
		Use:   "tap-gist <file|-> [file ...]",
		Short: "Create a GitHub gist from files or stdin",
		Args:  cobra.MinimumNArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			files := make(map[string][]byte)
			for _, arg := range args {
				var (
					data []byte
					err  error
					name string
				)
				if arg == "-" {
					data, err = io.ReadAll(cmd.InOrStdin())
					name = "stdin"
				} else {
					data, err = os.ReadFile(arg)
					name = filepath.Base(arg)
				}
				if err != nil {
					return err
				}
				files[name] = data
			}
			g, err := github.Create(cmd.Context(), flags.Token, flags.Message, flags.Public, files)
			if err != nil {
				return err
			}
			fmt.Fprintln(cmd.OutOrStdout(), g.HTMLURL)
			return nil
		},
	}
	cmd.Flags().StringVarP(&flags.Message, "message", "m", "", "gist description")
	cmd.Flags().BoolVar(&flags.Public, "public", false, "create public gist")
	cmd.Flags().StringVar(&flags.Token, "token", "", "GitHub token ($GITHUB_TOKEN if empty)")
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
