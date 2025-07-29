//go:build slow

package scheme

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
)

// Item represents a top-level Scheme definition discovered by the parser.
type Item struct {
	Kind   string   `json:"kind"`
	Name   string   `json:"name"`
	Params []string `json:"params,omitempty"`
}

// Program holds the parsed representation of a Scheme source file.
type Program struct {
	Source string `json:"source"`
	Items  []Item `json:"items"`
}

// Parse parses Scheme source using Racket's reader and returns a Program.
func Parse(src string) (*Program, error) {
	if _, err := exec.LookPath("racket"); err != nil {
		return nil, fmt.Errorf("racket not installed: %v", err)
	}

	srcFile, err := os.CreateTemp("", "schemesrc_*.scm")
	if err != nil {
		return nil, err
	}
	if _, err := srcFile.WriteString(src); err != nil {
		srcFile.Close()
		os.Remove(srcFile.Name())
		return nil, err
	}
	srcFile.Close()
	defer os.Remove(srcFile.Name())

	script := `#lang racket
(require json)
(define in (open-input-file (vector-ref (current-command-line-arguments) 0)))
(define (read-all p)
  (let loop ([lst '()])
    (let ([v (read p)])
      (if (eof-object? v)
          (reverse lst)
          (loop (cons v lst))))) )
(define (item f)
  (cond
    [(and (pair? f) (eq? (car f) 'define))
     (let ([d (cadr f)])
       (cond
         [(and (pair? d) (list? d))
          (hash 'kind "func"
                'name (symbol->string (car d))
                'params (map symbol->string (cdr d)))]
         [(symbol? d)
          (hash 'kind "var" 'name (symbol->string d))]
         [else #f]))]
    [else #f]))
(define forms (read-all in))
(close-input-port in)
(define items
  (for/list ([f forms] #:when (item f))
    (item f)))
(write-json items)
`

	scriptFile, err := os.CreateTemp("", "schemeparse_*.rkt")
	if err != nil {
		return nil, err
	}
	if _, err := scriptFile.WriteString(script); err != nil {
		scriptFile.Close()
		os.Remove(scriptFile.Name())
		return nil, err
	}
	scriptFile.Close()
	defer os.Remove(scriptFile.Name())

	cmd := exec.Command("racket", scriptFile.Name(), srcFile.Name())
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &out
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("racket parser: %v\n%s", err, out.String())
	}

	var items []Item
	if err := json.Unmarshal(out.Bytes(), &items); err != nil {
		return nil, err
	}
	return &Program{Source: src, Items: items}, nil
}
