package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"

	"github.com/charmbracelet/fang"
	"github.com/spf13/cobra"

	gh "mochi/tools/tap/github"
)

var (
	version   = "dev"
	gitCommit = ""
)

func newRootCmd() *cobra.Command {
	var desc string
	var public bool
	var name string

	cmd := &cobra.Command{
		Use:   "tap-gist [file ...]",
		Short: "Upload files to a GitHub gist",
		Args:  cobra.ArbitraryArgs,
		RunE: func(cmd *cobra.Command, args []string) error {
			files := make(map[string]gh.GistFile)
			if len(args) == 0 {
				data, err := io.ReadAll(cmd.InOrStdin())
				if err != nil {
					return err
				}
				files[name] = gh.GistFile{Content: string(data)}
			} else {
				for _, path := range args {
					b, err := os.ReadFile(path)
					if err != nil {
						return fmt.Errorf("%s: %w", path, err)
					}
					files[filepath.Base(path)] = gh.GistFile{Content: string(b)}
				}
			}

			url, err := gh.CreateGist(cmd.Context(), "", gh.CreateGistRequest{
				Description: desc,
				Public:      public,
				Files:       files,
			})
			if err != nil {
				return err
			}
			fmt.Fprintln(cmd.OutOrStdout(), url)
			return nil
		},
	}

	cmd.Flags().StringVar(&desc, "desc", "", "gist description")
	cmd.Flags().BoolVar(&public, "public", false, "create a public gist")
	cmd.Flags().StringVar(&name, "name", "stdin.txt", "filename when reading from stdin")
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
