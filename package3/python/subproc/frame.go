package subproc

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
)

// MaxFrameSize caps a single JSON-RPC payload at 16 MiB. Larger
// payloads typically indicate a runaway loop on either side; the cap
// is a safety net that prevents the worker (or host) from exhausting
// memory on a malformed peer.
const MaxFrameSize = 16 * 1024 * 1024

// WriteFrame writes payload as one newline-terminated frame. A
// trailing newline is appended; embedded newlines inside payload are
// rejected (the framing relies on '\n' as the terminator).
func WriteFrame(w io.Writer, payload []byte) error {
	if len(payload) > MaxFrameSize {
		return fmt.Errorf("subproc: frame size %d exceeds %d", len(payload), MaxFrameSize)
	}
	if bytes.IndexByte(payload, '\n') >= 0 {
		return fmt.Errorf("subproc: frame payload contains embedded newline")
	}
	buf := make([]byte, 0, len(payload)+1)
	buf = append(buf, payload...)
	buf = append(buf, '\n')
	_, err := w.Write(buf)
	return err
}

// ReadFrame reads one newline-terminated frame from br. The trailing
// newline is stripped before returning. io.EOF surfaces unchanged so
// the caller can distinguish a clean peer-close from a transport
// error.
func ReadFrame(br *bufio.Reader) ([]byte, error) {
	line, err := readLine(br)
	if err != nil {
		return nil, err
	}
	if len(line) > MaxFrameSize {
		return nil, fmt.Errorf("subproc: frame size %d exceeds %d", len(line), MaxFrameSize)
	}
	return line, nil
}

// readLine reads up to (and excluding) the next '\n'. It accumulates
// across bufio buffer boundaries via ReadSlice's 'isPrefix'-style
// behaviour. EOF on a partially-read line returns io.ErrUnexpectedEOF
// so the caller distinguishes a truncated frame from a clean
// peer-close.
func readLine(br *bufio.Reader) ([]byte, error) {
	var out []byte
	for {
		chunk, err := br.ReadSlice('\n')
		if err == bufio.ErrBufferFull {
			out = append(out, chunk...)
			continue
		}
		if err == io.EOF {
			if len(out) > 0 || len(chunk) > 0 {
				out = append(out, chunk...)
				return nil, io.ErrUnexpectedEOF
			}
			return nil, io.EOF
		}
		if err != nil {
			return nil, err
		}
		out = append(out, chunk...)
		// Strip the trailing '\n' (and the optional '\r' before it
		// so the framer survives CRLF translation on Windows).
		out = out[:len(out)-1]
		if n := len(out); n > 0 && out[n-1] == '\r' {
			out = out[:n-1]
		}
		return out, nil
	}
}
