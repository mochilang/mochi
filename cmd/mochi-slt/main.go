package main

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/charmbracelet/fang"
	"github.com/spf13/cobra"

	"mochi/tools/slt/logic"
)

var (
	version   = "dev"
	gitCommit = ""
)

// Default set of SLT files bundled with the repository.
var files = []string{"evidence/slt_lang_update.test", "select1.test", "select2.test"}

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
	var start int
	var end int
	var caseRange string
	var singleCase string
	cmd := &cobra.Command{
		Use:   "gen",
		Short: "Generate Mochi tests from SLT files",
		RunE: func(cmd *cobra.Command, args []string) error {
			if len(fileList) > 0 {
				files = fileList
			}
			if singleCase != "" {
				s := strings.TrimPrefix(singleCase, "case")
				if v, err := strconv.Atoi(s); err == nil {
					start = v
					end = v
				}
			}
			if caseRange != "" {
				parts := strings.Split(caseRange, "-")
				if len(parts) >= 1 {
					s := strings.TrimPrefix(parts[0], "case")
					if v, err := strconv.Atoi(s); err == nil {
						start = v
						if end == 0 {
							end = v
						}
					}
				}
				if len(parts) == 2 {
					e := strings.TrimPrefix(parts[1], "case")
					if v, err := strconv.Atoi(e); err == nil {
						end = v
					}
				}
			}
			err := logic.GenerateFiles(files, outDir, run, start, end)
			if err == nil && singleCase != "" && outDir != "" {
				fmt.Fprintf(cmd.OutOrStdout(), "generated %s case %s\n", outDir, singleCase)
			}
			return err
		},
	}
	cmd.Flags().StringSliceVarP(&fileList, "files", "f", files, "SLT files to generate")
	cmd.Flags().StringVarP(&outDir, "out", "o", "", "output directory")
	cmd.Flags().BoolVar(&run, "run", true, "run Mochi programs after generating")
	cmd.Flags().IntVar(&start, "start", 0, "first case to generate (1-indexed)")
	cmd.Flags().IntVar(&end, "end", 0, "last case to generate (inclusive)")
	cmd.Flags().StringVar(&caseRange, "cases", "", "case range, e.g. case5-case10")
	cmd.Flags().StringVar(&singleCase, "case", "", "single case to generate")
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
