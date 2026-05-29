package subproc

import (
	"bufio"
	"bytes"
	"errors"
	"io"
	"strings"
	"testing"
)

func TestWriteFrameAppendsNewline(t *testing.T) {
	var buf bytes.Buffer
	if err := WriteFrame(&buf, []byte(`{"a":1}`)); err != nil {
		t.Fatalf("WriteFrame: %v", err)
	}
	if buf.String() != `{"a":1}`+"\n" {
		t.Errorf("got %q", buf.String())
	}
}

func TestWriteFrameRejectsEmbeddedNewline(t *testing.T) {
	var buf bytes.Buffer
	err := WriteFrame(&buf, []byte("a\nb"))
	if err == nil || !strings.Contains(err.Error(), "newline") {
		t.Fatalf("expected newline error, got %v", err)
	}
}

func TestReadFrameStripsTrailingNewline(t *testing.T) {
	br := bufio.NewReader(strings.NewReader(`{"a":1}` + "\n"))
	got, err := ReadFrame(br)
	if err != nil {
		t.Fatalf("ReadFrame: %v", err)
	}
	if string(got) != `{"a":1}` {
		t.Errorf("got %q", got)
	}
}

func TestReadFrameMultipleFrames(t *testing.T) {
	br := bufio.NewReader(strings.NewReader("one\ntwo\nthree\n"))
	want := []string{"one", "two", "three"}
	for _, w := range want {
		got, err := ReadFrame(br)
		if err != nil {
			t.Fatalf("ReadFrame: %v", err)
		}
		if string(got) != w {
			t.Errorf("got %q want %q", got, w)
		}
	}
	if _, err := ReadFrame(br); !errors.Is(err, io.EOF) {
		t.Errorf("expected EOF, got %v", err)
	}
}

func TestReadFrameHandlesCRLF(t *testing.T) {
	br := bufio.NewReader(strings.NewReader("hello\r\n"))
	got, err := ReadFrame(br)
	if err != nil {
		t.Fatalf("ReadFrame: %v", err)
	}
	if string(got) != "hello" {
		t.Errorf("CRLF stripping wrong: %q", got)
	}
}

func TestReadFrameTruncated(t *testing.T) {
	br := bufio.NewReader(strings.NewReader("partial"))
	_, err := ReadFrame(br)
	if !errors.Is(err, io.ErrUnexpectedEOF) {
		t.Errorf("expected ErrUnexpectedEOF, got %v", err)
	}
}

func TestReadFrameLargePayload(t *testing.T) {
	payload := strings.Repeat("x", 128*1024)
	br := bufio.NewReader(strings.NewReader(payload + "\n"))
	got, err := ReadFrame(br)
	if err != nil {
		t.Fatalf("ReadFrame: %v", err)
	}
	if len(got) != len(payload) {
		t.Errorf("length mismatch: got %d want %d", len(got), len(payload))
	}
}

func TestWriteFrameSizeCap(t *testing.T) {
	var buf bytes.Buffer
	payload := bytes.Repeat([]byte{'x'}, MaxFrameSize+1)
	err := WriteFrame(&buf, payload)
	if err == nil || !strings.Contains(err.Error(), "exceeds") {
		t.Errorf("expected size cap error, got %v", err)
	}
}
