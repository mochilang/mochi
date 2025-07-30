package scheme

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"
)

// Node represents a Scheme expression as parsed by the inspector script.
type Node struct {
	Atom string  `json:"atom,omitempty"`
	List []*Node `json:"list,omitempty"`
}

// Program represents a parsed Scheme source file.
type Program struct {
	Forms []*Node `json:"forms"`
}

const inspectScript = `
(define (read-all port)
  (let loop ((acc '()))
    (let ((x (read port)))
      (if (eof-object? x)
          (reverse acc)
          (loop (cons x acc))))) )

(define (to-string x)
  (cond
    ((symbol? x) (symbol->string x))
    ((number? x) (number->string x))
    ((boolean? x) (if x "#t" "#f"))
    ((string? x) x)
    (else (let ((out (open-output-string)))
            (write x out)
            (get-output-string out)))) )

(define (escape-json s)
  (let* ((out (open-output-string))
         (_ (write s out))
         (res (get-output-string out)))
    (substring res 1 (- (string-length res) 1))))

(define (emit-atom x out)
  (display "{\"atom\":" out)
  (display "\"" out)
  (display (escape-json (to-string x)) out)
  (display "\"}" out))

(define (emit-node x out)
  (if (pair? x)
      (begin
        (display "{\"list\":[" out)
        (let loop ((ls x) (first #t))
          (cond
            ((null? ls))
            (else
             (if (not first) (display "," out))
             (emit-node (car ls) out)
             (loop (cdr ls) #f))))
        (display "]}" out))
      (emit-atom x out)))

(define (emit-program forms out)
  (display "{\"forms\":[" out)
  (let loop ((ls forms) (first #t))
    (cond
      ((null? ls))
      (else
       (if (not first) (display "," out))
       (emit-node (car ls) out)
       (loop (cdr ls) #f))))
  (display "]}" out))

(define (main args)
  (let* ((path (car args))
         (port (open-input-file path))
         (forms (read-all port)))
    (emit-program forms (current-output-port))
    (newline)))

(main (cdr (command-line)))`

var schemeCmd = "chibi-scheme"

// Inspect parses the given Scheme source code and returns a Program describing
// its AST.
func Inspect(src string) (*Program, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	scriptFile, err := os.CreateTemp("", "scheme_inspect_*.scm")
	if err != nil {
		return nil, err
	}
	defer os.Remove(scriptFile.Name())
	if _, err := scriptFile.WriteString(inspectScript); err != nil {
		scriptFile.Close()
		return nil, err
	}
	scriptFile.Close()

	srcFile, err := os.CreateTemp("", "scheme_src_*.scm")
	if err != nil {
		return nil, err
	}
	if _, err := srcFile.WriteString(src); err != nil {
		srcFile.Close()
		return nil, err
	}
	srcFile.Close()
	defer os.Remove(srcFile.Name())

	cmd := exec.CommandContext(ctx, schemeCmd, "-q", "-m", "chibi.json", scriptFile.Name(), srcFile.Name())
	var out, errBuf bytes.Buffer
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
