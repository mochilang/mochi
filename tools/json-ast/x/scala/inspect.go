package scala

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"time"
)

// Program represents a parsed Scala source file.
type Program struct {
	Tree string `json:"tree"`
}

const scalaScript = `import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
object Main extends App {
  val code = scala.io.Source.stdin.mkString
  val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
  val tree = tb.parse(code)
  println(showRaw(tree))
}`

var scalacCmd = "scalac"
var scalaCmd = "scala"

var (
	compileOnce sync.Once
	compiledDir string
	compileErr  error
)

// Inspect parses the given Scala source code and returns its AST using the official Scala parser.
func compileScalaHelper() {
	compiledDir, compileErr = os.MkdirTemp("", "scala-ast")
	if compileErr != nil {
		return
	}
	scriptPath := filepath.Join(compiledDir, "Main.scala")
	if err := os.WriteFile(scriptPath, []byte(scalaScript), 0644); err != nil {
		compileErr = err
		return
	}
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, scalacCmd, "-d", compiledDir, scriptPath)
	var errBuf bytes.Buffer
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		compileErr = fmt.Errorf("%v: %s", err, strings.TrimSpace(errBuf.String()))
	}
}

func Inspect(src string) (*Program, error) {
	compileOnce.Do(compileScalaHelper)
	if compileErr != nil {
		return nil, compileErr
	}

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, scalaCmd, "-cp", compiledDir, "Main")
	cmd.Stdin = strings.NewReader(src)
	var out, errBuf2 bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf2
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(errBuf2.String())
		if msg != "" {
			return nil, fmt.Errorf("%v: %s", err, msg)
		}
		return nil, err
	}

	return &Program{Tree: strings.TrimSpace(out.String())}, nil
}
