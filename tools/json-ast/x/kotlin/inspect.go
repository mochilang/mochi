package kotlin

import (
	"archive/zip"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
)

// Program represents a parsed Kotlin source file.
type Program struct {
	File any `json:"file"`
}

var (
	parserOnce sync.Once
	parserJar  string
	parserCP   string
	parserErr  error
)

// Inspect parses Kotlin source code using kotlinc and returns its AST.
func Inspect(src string) (*Program, error) {
	if err := ensureParser(); err != nil {
		return nil, err
	}
	cmd := exec.Command("java", "-cp", parserJar+string(os.PathListSeparator)+parserCP, "ParseKt")
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(errBuf.String())
		if msg != "" {
			return nil, fmt.Errorf("%v: %s", err, msg)
		}
		return nil, err
	}
	var prog Program
	if err := json.Unmarshal(out.Bytes(), &prog); err != nil {
		return nil, err
	}
	return &prog, nil
}

func ensureParser() error {
	parserOnce.Do(func() {
		kpath, err := ensureKotlinc()
		if err != nil {
			parserErr = err
			return
		}
		os.Setenv("PATH", filepath.Dir(kpath)+string(os.PathListSeparator)+os.Getenv("PATH"))
		cp, err := kotlinClasspath()
		if err != nil {
			parserErr = err
			return
		}
		jars, err := ensureKastree()
		if err != nil {
			parserErr = err
			return
		}
		parserCP = strings.Join(append(jars, cp), string(os.PathListSeparator))
		root, err := repoRoot()
		if err != nil {
			parserErr = err
			return
		}
		srcPath := filepath.Join(root, "tools", "json-ast", "x", "kotlin", "parse.kt")
		tmpDir, err := os.MkdirTemp("", "kt-parser")
		if err != nil {
			parserErr = err
			return
		}
		jar := filepath.Join(tmpDir, "parser.jar")
		cmd := exec.Command(kpath, srcPath, "-cp", parserCP, "-d", jar)
		var out bytes.Buffer
		cmd.Stdout = &out
		cmd.Stderr = &out
		if err := cmd.Run(); err != nil {
			parserErr = fmt.Errorf("kotlinc: %v: %s", err, out.String())
			return
		}
		parserJar = jar
	})
	return parserErr
}

func ensureKastree() ([]string, error) {
	version := "0.4.0"
	kotlinVer := "1.3.21"
	libs := []struct{ g, a, v string }{
		{"com.github.cretz.kastree", "kastree-ast-common", version},
		{"com.github.cretz.kastree", "kastree-ast-jvm", version},
		{"com.github.cretz.kastree", "kastree-ast-psi", version},
		{"org.jetbrains.kotlin", "kotlin-compiler-embeddable", kotlinVer},
		{"org.jetbrains.kotlin", "kotlin-stdlib-jdk8", kotlinVer},
		{"org.jetbrains.kotlin", "kotlin-stdlib", kotlinVer},
		{"org.jetbrains.kotlin", "kotlin-reflect", kotlinVer},
		{"org.jetbrains.kotlin", "kotlin-script-runtime", kotlinVer},
		{"org.jetbrains.intellij.deps", "trove4j", "1.0.20181211"},
	}
	dir := filepath.Join(os.TempDir(), "kastree")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, err
	}
	var jars []string
	for _, dep := range libs {
		path := filepath.Join(dir, dep.a+"-"+dep.v+".jar")
		if _, err := os.Stat(path); err != nil {
			groupPath := strings.ReplaceAll(dep.g, ".", "/")
			url := fmt.Sprintf("https://repo1.maven.org/maven2/%s/%s/%s/%s-%s.jar", groupPath, dep.a, dep.v, dep.a, dep.v)
			if err := downloadFile(url, path); err != nil {
				return nil, err
			}
		}
		jars = append(jars, path)
	}
	return jars, nil
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

func ensureKotlinc() (string, error) {
	if path, err := exec.LookPath("kotlinc"); err == nil {
		return path, nil
	}
	dir := filepath.Join(os.TempDir(), "kotlinc")
	bin := filepath.Join(dir, "bin", "kotlinc")
	if _, err := os.Stat(bin); err == nil {
		return bin, nil
	}
	url := "https://github.com/JetBrains/kotlin/releases/download/v1.3.21/kotlin-compiler-1.3.21.zip"
	resp, err := http.Get(url)
	if err != nil {
		return "", fmt.Errorf("download kotlin: %w", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return "", fmt.Errorf("download kotlin: %s", resp.Status)
	}
	data, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}
	zr, err := zip.NewReader(bytes.NewReader(data), int64(len(data)))
	if err != nil {
		return "", err
	}
	for _, f := range zr.File {
		name := strings.TrimPrefix(f.Name, "kotlinc/")
		if name == "" {
			continue
		}
		dest := filepath.Join(dir, name)
		if f.FileInfo().IsDir() {
			os.MkdirAll(dest, f.Mode())
			continue
		}
		if err := os.MkdirAll(filepath.Dir(dest), 0o755); err != nil {
			return "", err
		}
		rc, err := f.Open()
		if err != nil {
			return "", err
		}
		out, err := os.OpenFile(dest, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, f.Mode())
		if err != nil {
			rc.Close()
			return "", err
		}
		if _, err := io.Copy(out, rc); err != nil {
			out.Close()
			rc.Close()
			return "", err
		}
		out.Close()
		rc.Close()
	}
	return bin, nil
}

func kotlinClasspath() (string, error) {
	kpath, err := exec.LookPath("kotlinc")
	if err != nil {
		return "", err
	}
	kpath, _ = filepath.EvalSymlinks(kpath)
	home := filepath.Join(filepath.Dir(kpath), "..")
	pattern := filepath.Join(home, "lib", "*.jar")
	jars, err := filepath.Glob(pattern)
	if err != nil {
		return "", err
	}
	if len(jars) == 0 {
		return "", fmt.Errorf("no kotlin libs found")
	}
	return strings.Join(jars, string(os.PathListSeparator)), nil
}

func repoRoot() (string, error) {
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
