package main

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"github.com/charmbracelet/fang"
	"github.com/spf13/cobra"

	"mochi/tools/slt/logic"
)

var (
	version   = "dev"
	gitCommit = ""
)

var files = []string{"evidence/slt_lang_update.test", "test/select1.test"}

func fetchCmd() *cobra.Command {
	var repo string
	var force bool
	var fileList []string
	cmd := &cobra.Command{
		Use:   "fetch",
		Short: "Download SQLLogicTest files",
		RunE: func(cmd *cobra.Command, args []string) error {
			if len(fileList) > 0 {
				files = fileList
			}
			return logic.Fetch(repo, files, force)
		},
	}
	cmd.Flags().StringSliceVarP(&fileList, "files", "f", files, "files to fetch")
	cmd.Flags().StringVar(&repo, "repo", "https://raw.githubusercontent.com/gregrahn/sqllogictest/master", "base repository URL")
	cmd.Flags().BoolVar(&force, "force", false, "force download even if file exists")
	return cmd
}

func genCmd() *cobra.Command {
	var outDir string
	var run bool
	var fileList []string
	var from int
	var to int
	cmd := &cobra.Command{
		Use:   "gen",
		Short: "Generate Mochi tests from SLT files",
		RunE: func(cmd *cobra.Command, args []string) error {
			if len(fileList) > 0 {
				files = fileList
			}
			return logic.GenerateFiles(files, outDir, run, from, to)
		},
	}
	cmd.Flags().StringSliceVarP(&fileList, "files", "f", files, "SLT files to generate")
	cmd.Flags().StringVarP(&outDir, "out", "o", "", "output directory")
	cmd.Flags().BoolVar(&run, "run", true, "run Mochi programs after generating")
	cmd.Flags().IntVar(&from, "from", 1, "first case number to generate")
	cmd.Flags().IntVar(&to, "to", 0, "last case number to generate (0 means no limit)")
	return cmd
}

func listCmd() *cobra.Command {
	var file string
	cmd := &cobra.Command{
		Use:   "list",
		Short: "List test cases in an SLT file",
		RunE: func(cmd *cobra.Command, args []string) error {
			root, err := logic.FindRepoRoot()
			if err != nil {
				return err
			}
			dir := filepath.Join(root, "tests/dataset/slt")
			if file == "" {
				if len(files) > 0 {
					file = files[0]
				} else {
					return fmt.Errorf("no file specified")
				}
			}
			cases, err := logic.ParseFile(filepath.Join(dir, file))
			if err != nil {
				return err
			}
			for _, c := range cases {
				fmt.Fprintln(cmd.OutOrStdout(), c.Name)
			}
			return nil
		},
	}
	cmd.Flags().StringVarP(&file, "file", "f", "", "SLT file to inspect")
	return cmd
}

func newRootCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "mochi-slt",
		Short: "SQLLogicTest utilities",
	}
	cmd.AddCommand(fetchCmd(), genCmd(), listCmd())
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
