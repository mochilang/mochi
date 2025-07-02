package main

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"

	"github.com/charmbracelet/fang"
	"github.com/spf13/cobra"

	"mochi/parser"
	mod "mochi/runtime/mod"
	"mochi/runtime/vm"
	"mochi/tools/slt/logic"
	"mochi/types"
)

var (
	version   = "dev"
	gitCommit = ""
)

var files = []string{"slt_lang_update.test"}

func findRepoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", fmt.Errorf("go.mod not found")
}

func downloadFile(url, path string) error {
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("download %s: %s", url, resp.Status)
	}
	f, err := os.Create(path)
	if err != nil {
		return err
	}
	defer f.Close()
	_, err = io.Copy(f, resp.Body)
	return err
}

func runMochi(src string) (string, error) {
	prog, err := parser.ParseString(src)
	if err != nil {
		return "", err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return "", errs[0]
	}
	modRoot, errRoot := mod.FindRoot(".")
	if errRoot != nil {
		modRoot = "."
	}
	os.Setenv("MOCHI_ROOT", modRoot)
	p, err := vm.Compile(prog, env)
	if err != nil {
		return "", err
	}
	var buf bytes.Buffer
	m := vm.New(p, &buf)
	if err := m.Run(); err != nil {
		return "", err
	}
	return strings.TrimSpace(buf.String()), nil
}

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
			root, err := findRepoRoot()
			if err != nil {
				return err
			}
			dir := filepath.Join(root, "tests/dataset/slt")
			if err := os.MkdirAll(dir, 0o755); err != nil {
				return err
			}
			for _, f := range files {
				local := filepath.Join(dir, f)
				if _, err := os.Stat(local); err == nil && !force {
					continue
				}
				url := repo + "/" + f
				if err := downloadFile(url, local); err != nil {
					return err
				}
			}
			return nil
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
	cmd := &cobra.Command{
		Use:   "gen",
		Short: "Generate Mochi tests from SLT files",
		RunE: func(cmd *cobra.Command, args []string) error {
			if len(fileList) > 0 {
				files = fileList
			}
			root, err := findRepoRoot()
			if err != nil {
				return err
			}
			dir := filepath.Join(root, "tests/dataset/slt")
			if outDir == "" {
				outDir = filepath.Join(dir, "out")
			}
			if err := os.MkdirAll(outDir, 0o755); err != nil {
				return err
			}
			for _, f := range files {
				local := filepath.Join(dir, f)
				cases, err := logic.ParseFile(local)
				if err != nil {
					return err
				}
				testDir := filepath.Join(outDir, strings.TrimSuffix(f, ".test"))
				if err := os.MkdirAll(testDir, 0o755); err != nil {
					return err
				}
				for _, c := range cases {
					code := logic.Generate(c)
					srcPath := filepath.Join(testDir, c.Name+".mochi")
					if err := os.WriteFile(srcPath, []byte(code), 0o644); err != nil {
						return err
					}
					if run {
						out, err := runMochi(code)
						if err != nil {
							return err
						}
						if err := os.WriteFile(filepath.Join(testDir, c.Name+".out"), []byte(out+"\n"), 0o644); err != nil {
							return err
						}
					}
				}
			}
			return nil
		},
	}
	cmd.Flags().StringSliceVarP(&fileList, "files", "f", files, "SLT files to generate")
	cmd.Flags().StringVarP(&outDir, "out", "o", "", "output directory")
	cmd.Flags().BoolVar(&run, "run", true, "run Mochi programs after generating")
	return cmd
}

func listCmd() *cobra.Command {
	var file string
	cmd := &cobra.Command{
		Use:   "list",
		Short: "List test cases in an SLT file",
		RunE: func(cmd *cobra.Command, args []string) error {
			root, err := findRepoRoot()
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
