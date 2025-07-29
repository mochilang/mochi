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
	Kind   string      `json:"kind"`
	Name   string      `json:"name"`
	Params []string    `json:"params,omitempty"`
	Value  interface{} `json:"value,omitempty"`
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
(define (module-name spec)
  (cond
    [(and (pair? spec)
          (or (eq? (car spec) 'only)
              (eq? (car spec) 'rename)))
     (module-name (cadr spec))]
    [(pair? spec)
     (string-join (map (lambda (x)
                         (cond [(symbol? x) (symbol->string x)]
                               [(number? x) (number->string x)]
                               [else ""]))
                       spec)
                "_")]
    [(symbol? spec)
     (symbol->string spec)]
    [else #f]))

(define (expr->json e)
  (cond
    [(number? e) e]
    [(string? e) e]
    [(boolean? e) (if e #t #f)]
    [(symbol? e) (hash 'var (symbol->string e))]
    [(and (pair? e) (eq? (car e) 'list))
     (hash 'list (map expr->json (cdr e)))]
    [(pair? e)
     (hash 'call (symbol->string (car e))
           'args (map expr->json (cdr e)))]
    [else #f]))

(define (item f)
  (cond
    [(and (pair? f) (eq? (car f) 'define))
     (let ([name (cadr f)]
           [body (cddr f)])
       (cond
         [(and (pair? name) (list? name))
          (hash 'kind "func"
                'name (symbol->string (car name))
                'params (map symbol->string (cdr name)))]
         [(and (symbol? name)
               (= (length body) 1)
               (pair? (car body))
               (eq? (caar body) 'lambda)
               (list? (cadar body)))
          (hash 'kind "func"
                'name (symbol->string name)
                'params (map symbol->string (cadar body)))]
         [(symbol? name)
          (let ([val (car body)])
            (hash 'kind "var" 'name (symbol->string name)
                  'value (expr->json val)))]
         [else #f]))]
    [(and (pair? f) (eq? (car f) 'import))
     (for/list ([s (cdr f)] #:when (module-name s))
       (hash 'kind "import" 'name (module-name s)))]
    [(and (pair? f) (eq? (car f) 'begin))
     (for/fold ([acc '()]) ([x (cdr f)])
       (define it (item x))
       (cond
         [(list? it) (append acc it)]
         [it (append acc (list it))]
         [else acc]))]
    [(and (pair? f) (eq? (car f) 'set!))
     (cond
       [(symbol? (cadr f))
        (let ([val (caddr f)])
          (hash 'kind "assign" 'name (symbol->string (cadr f))
                'value (expr->json val)))]
       [else #f])]
    [(and (pair? f) (eq? (car f) 'display))
     (let ([val (cadr f)])
       (hash 'kind "print"
             'value (expr->json val)))]
    [(and (pair? f) (eq? (car f) 'newline))
     (hash 'kind "print" 'value "")]
    [else #f]))
(define forms (read-all in))
(close-input-port in)
(define items
  (for/fold ([acc '()]) ([f forms])
    (define it (item f))
    (cond
      [(list? it) (append acc it)]
      [it (append acc (list it))]
      [else acc])))
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
