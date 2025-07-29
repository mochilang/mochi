package clj

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"
	"time"
)

// Program represents a parsed Clojure source file.
type Program struct {
	Forms []any `json:"forms"`
}

const bbScript = `(require '[clojure.tools.reader :as r])
(require '[clojure.tools.reader.reader-types :as rt])
(require '[cheshire.core :as json])
(let [rdr (rt/string-push-back-reader (slurp *in*))]
  (loop [acc []]
    (let [form (try (r/read {:eof ::eof} rdr)
                    (catch Exception e {:error (.getMessage e)}))]
      (if (= form ::eof)
        (println (json/generate-string acc))
        (recur (conj acc form))))) )`

var babashkaCmd = "bb"

// Inspect parses the given Clojure source code and returns a Program describing its forms.
func Inspect(src string) (*Program, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, babashkaCmd, "-e", bbScript)
	cmd.Stdin = strings.NewReader(src)
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
	var forms []any
	if err := json.Unmarshal(out.Bytes(), &forms); err != nil {
		return nil, err
	}
	return &Program{Forms: forms}, nil
}
