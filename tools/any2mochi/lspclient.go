package any2mochi

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os/exec"
	"strings"
	"time"
)

// LSP message wrapper
type lspMessage struct {
	Jsonrpc string      `json:"jsonrpc"`
	ID      int         `json:"id,omitempty"`
	Method  string      `json:"method,omitempty"`
	Params  interface{} `json:"params,omitempty"`
	Result  interface{} `json:"result,omitempty"`
	Error   *struct {
		Code    int    `json:"code"`
		Message string `json:"message"`
	} `json:"error,omitempty"`
}

// LSP initialize params
type initializeParams struct {
	ProcessID    int                    `json:"processId"`
	RootURI      string                 `json:"rootUri"`
	Capabilities map[string]interface{} `json:"capabilities"`
}

type textDocumentItem struct {
	URI        string `json:"uri"`
	LanguageID string `json:"languageId"`
	Version    int    `json:"version"`
	Text       string `json:"text"`
}

type didOpenParams struct {
	TextDocument textDocumentItem `json:"textDocument"`
}

type diagnostic struct {
	Range struct {
		Start struct {
			Line      int `json:"line"`
			Character int `json:"character"`
		} `json:"start"`
		End struct {
			Line      int `json:"line"`
			Character int `json:"character"`
		} `json:"end"`
	} `json:"range"`
	Severity int    `json:"severity"`
	Message  string `json:"message"`
}

type publishDiagnostics struct {
	URI         string       `json:"uri"`
	Diagnostics []diagnostic `json:"diagnostics"`
}

// sendLSPRequest formats the message and writes LSP headers + JSON
func sendLSPRequest(stdin io.Writer, msg lspMessage) error {
	payload, _ := json.Marshal(msg)
	fmt.Fprintf(stdin, "Content-Length: %d\r\n\r\n", len(payload))
	_, err := stdin.Write(payload)
	return err
}

// readLSPResponse parses an LSP JSON response with headers
func readLSPResponse(stdout io.Reader) ([]byte, error) {
	var headers [2]string
	for i := range headers {
		buf := make([]byte, 256)
		n, err := stdout.Read(buf)
		if err != nil {
			return nil, err
		}
		headers[i] = string(buf[:n])
		if strings.TrimSpace(headers[i]) == "" {
			break
		}
	}
	// Expect Content-Length
	var contentLength int
	for _, h := range headers {
		if strings.HasPrefix(h, "Content-Length:") {
			fmt.Sscanf(h, "Content-Length: %d", &contentLength)
			break
		}
	}
	body := make([]byte, contentLength)
	_, err := io.ReadFull(stdout, body)
	return body, err
}

// RunLanguageServer starts the server and parses source code input
func RunLanguageServer(serverCmd []string, languageID string, source string) error {
	cmd := exec.Command(serverCmd[0], serverCmd[1:]...)
	stdin, _ := cmd.StdinPipe()
	stdout, _ := cmd.StdoutPipe()
	cmd.Stderr = nil

	if err := cmd.Start(); err != nil {
		return err
	}

	uri := "file:///virtual_file." + languageID
	sendLSPRequest(stdin, lspMessage{
		Jsonrpc: "2.0",
		ID:      1,
		Method:  "initialize",
		Params: initializeParams{
			ProcessID:    1,
			RootURI:      "file:///",
			Capabilities: map[string]interface{}{},
		},
	})
	readLSPResponse(stdout) // skip initialize result

	sendLSPRequest(stdin, lspMessage{
		Jsonrpc: "2.0",
		Method:  "textDocument/didOpen",
		Params: didOpenParams{
			TextDocument: textDocumentItem{
				URI:        uri,
				LanguageID: languageID,
				Version:    1,
				Text:       source,
			},
		},
	})

	// Wait for publishDiagnostics (delivered async)
	for {
		body, err := readLSPResponse(stdout)
		if err != nil {
			return err
		}
		if bytes.Contains(body, []byte("textDocument/publishDiagnostics")) {
			var msg struct {
				Method string             `json:"method"`
				Params publishDiagnostics `json:"params"`
			}
			_ = json.Unmarshal(body, &msg)

			fmt.Println("\U0001F4C4 Diagnostics:")
			for _, d := range msg.Params.Diagnostics {
				fmt.Printf("- Line %d:%d \u2192 %s\n", d.Range.Start.Line+1, d.Range.Start.Character+1, d.Message)
			}
			break
		}
		time.Sleep(50 * time.Millisecond)
	}
	return nil
}
