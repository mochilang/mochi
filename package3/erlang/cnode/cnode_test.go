package cnode

import (
	"crypto/md5"
	"encoding/binary"
	"fmt"
	"io"
	"net"
	"strings"
	"testing"
	"time"

	"mochi/package3/erlang/etf"
)

// ── NodeName ──────────────────────────────────────────────────────────────────

func TestParseNodeName_Valid(t *testing.T) {
	nn, err := ParseNodeName("mynode@localhost")
	if err != nil {
		t.Fatalf("ParseNodeName: %v", err)
	}
	if nn.Name != "mynode" || nn.Host != "localhost" {
		t.Errorf("Name=%q Host=%q", nn.Name, nn.Host)
	}
}

func TestParseNodeName_FullHost(t *testing.T) {
	nn, err := ParseNodeName("mochi@192.168.1.42")
	if err != nil {
		t.Fatalf("ParseNodeName: %v", err)
	}
	if nn.Host != "192.168.1.42" {
		t.Errorf("Host = %q", nn.Host)
	}
}

func TestParseNodeName_NoAt(t *testing.T) {
	_, err := ParseNodeName("nodename")
	if err == nil {
		t.Error("expected error for name without @")
	}
}

func TestParseNodeName_EmptyName(t *testing.T) {
	_, err := ParseNodeName("@host")
	if err == nil {
		t.Error("expected error for empty name part")
	}
}

func TestParseNodeName_EmptyHost(t *testing.T) {
	_, err := ParseNodeName("name@")
	if err == nil {
		t.Error("expected error for empty host part")
	}
}

func TestNodeName_String(t *testing.T) {
	nn := NodeName{Name: "foo", Host: "bar.example.com"}
	if nn.String() != "foo@bar.example.com" {
		t.Errorf("String = %q", nn.String())
	}
}

// ── EPMD client ───────────────────────────────────────────────────────────────

// fakeEPMDServer returns a Dialer that serves a single PORT_PLEASE2_REQ.
func fakeEPMDServer(t *testing.T, nodeName string, port uint16) Dialer {
	return func(network, addr string) (net.Conn, error) {
		server, client := net.Pipe()
		go func() {
			defer server.Close()
			// Read request: 2-byte len + 0x7A + name
			var lenBuf [2]byte
			if _, err := io.ReadFull(server, lenBuf[:]); err != nil {
				return
			}
			reqLen := int(binary.BigEndian.Uint16(lenBuf[:]))
			body := make([]byte, reqLen)
			if _, err := io.ReadFull(server, body); err != nil {
				return
			}
			if body[0] != 0x7A {
				t.Errorf("expected PORT_PLEASE2_REQ=0x7A, got 0x%02x", body[0])
				return
			}
			gotName := string(body[1:])
			// Write response: 0x77 + result(0=ok) + port(2) + 7_ignored + name_len(2) + name + 0(extra_len)
			resp := make([]byte, 2+2+7+2+len(nodeName)+2)
			resp[0] = 0x77
			resp[1] = 0 // ok
			binary.BigEndian.PutUint16(resp[2:4], port)
			// node_type + protocol + highest_version + lowest_version
			resp[4] = 77  // hidden node
			resp[5] = 0   // tcp/ip v4
			binary.BigEndian.PutUint16(resp[6:8], 6)
			binary.BigEndian.PutUint16(resp[8:10], 5)
			binary.BigEndian.PutUint16(resp[10:12], uint16(len(gotName)))
			copy(resp[12:], gotName)
			binary.BigEndian.PutUint16(resp[12+len(gotName):], 0) // extra_len=0
			server.Write(resp)
		}()
		return client, nil
	}
}

func TestEPMDClient_LookupNode(t *testing.T) {
	c := &EPMDClient{Dial: fakeEPMDServer(t, "mynode", 54321)}
	port, err := c.LookupNode("localhost", "mynode")
	if err != nil {
		t.Fatalf("LookupNode: %v", err)
	}
	if port != 54321 {
		t.Errorf("port = %d, want 54321", port)
	}
}

func TestEPMDClient_DialError(t *testing.T) {
	c := &EPMDClient{Dial: func(_, _ string) (net.Conn, error) {
		return nil, fmt.Errorf("refused")
	}}
	_, err := c.LookupNode("localhost", "nonode")
	if err == nil || !strings.Contains(err.Error(), "EPMD") {
		t.Errorf("expected EPMD error, got: %v", err)
	}
}

// ── handshake helpers ─────────────────────────────────────────────────────────

func TestComputeDigest(t *testing.T) {
	// Verify against a known good vector: MD5("secretcookie" + big-endian 12345)
	var challenge uint32 = 12345
	var buf [4]byte
	binary.BigEndian.PutUint32(buf[:], challenge)
	h := md5.New()
	h.Write([]byte("secretcookie"))
	h.Write(buf[:])
	want := [16]byte{}
	copy(want[:], h.Sum(nil))
	got := computeDigest("secretcookie", challenge)
	if got != want {
		t.Errorf("digest mismatch: got %x, want %x", got, want)
	}
}

func TestWriteReadFrame(t *testing.T) {
	server, client := net.Pipe()
	defer server.Close()
	defer client.Close()

	payload := []byte("hello world")
	go func() {
		_ = writeFrame(server, payload)
	}()
	got, err := readFrame(client)
	if err != nil {
		t.Fatalf("readFrame: %v", err)
	}
	if string(got) != string(payload) {
		t.Errorf("got %q, want %q", got, payload)
	}
}

func TestReadFrame_EmptyBody(t *testing.T) {
	server, client := net.Pipe()
	defer server.Close()
	defer client.Close()

	go func() { _ = writeFrame(server, []byte{}) }()
	got, err := readFrame(client)
	if err != nil {
		t.Fatalf("readFrame empty: %v", err)
	}
	if len(got) != 0 {
		t.Errorf("expected empty, got %v", got)
	}
}

// ── full handshake simulation ─────────────────────────────────────────────────

// serveHandshake runs a fake Erlang node server that completes the 'N'
// handshake with cookie. It does NOT close conn; the caller owns the lifetime.
func serveHandshake(t *testing.T, conn net.Conn, cookie, nodeName string) {
	t.Helper()
	conn.SetDeadline(time.Now().Add(5 * time.Second))

	// Step 1: recv send_name from client
	body, err := readFrame(conn)
	if err != nil {
		t.Logf("serveHandshake: recv send_name: %v", err)
		return
	}
	if len(body) < 1 || body[0] != 'N' {
		t.Logf("serveHandshake: expected 'N', got 0x%02x", body[0])
		return
	}

	// Step 2: send status "ok"
	statusBody := append([]byte{'s'}, []byte("ok")...)
	if err := writeFrame(conn, statusBody); err != nil {
		t.Logf("serveHandshake: send status: %v", err)
		return
	}

	// Step 3: send challenge 'N'
	serverChallenge := uint32(0xDEADBEEF)
	nameBytes := []byte(nodeName)
	chalBody := make([]byte, 1+8+4+4+2+len(nameBytes))
	chalBody[0] = 'N'
	binary.BigEndian.PutUint64(chalBody[1:9], 0x4000_0000)  // flags
	binary.BigEndian.PutUint32(chalBody[9:13], serverChallenge)
	binary.BigEndian.PutUint32(chalBody[13:17], 1) // creation
	binary.BigEndian.PutUint16(chalBody[17:19], uint16(len(nameBytes)))
	copy(chalBody[19:], nameBytes)
	if err := writeFrame(conn, chalBody); err != nil {
		t.Logf("serveHandshake: send challenge: %v", err)
		return
	}

	// Step 4: recv challenge_reply from client
	replyBody, err := readFrame(conn)
	if err != nil {
		t.Logf("serveHandshake: recv challenge_reply: %v", err)
		return
	}
	if len(replyBody) < 21 || replyBody[0] != 'r' {
		t.Logf("serveHandshake: expected 'r', got 0x%02x (len=%d)", replyBody[0], len(replyBody))
		return
	}
	clientChallenge := binary.BigEndian.Uint32(replyBody[1:5])
	expectedClientDigest := computeDigest(cookie, serverChallenge)
	var gotDigest [16]byte
	copy(gotDigest[:], replyBody[5:21])
	if gotDigest != expectedClientDigest {
		t.Logf("serveHandshake: client digest mismatch")
	}

	// Step 5: send challenge_ack
	ackDigest := computeDigest(cookie, clientChallenge)
	ackBody := make([]byte, 1+16)
	ackBody[0] = 'a'
	copy(ackBody[1:], ackDigest[:])
	if err := writeFrame(conn, ackBody); err != nil {
		t.Logf("serveHandshake: send ack: %v", err)
	}
}

func TestHandshake_Success(t *testing.T) {
	server, client := net.Pipe()
	go func() {
		serveHandshake(t, server, "mycookie", "remotenode@host")
		server.Close()
	}()

	dc, err := Handshake(client, "cnode@host", "mycookie")
	if err != nil {
		t.Fatalf("Handshake: %v", err)
	}
	if dc.PeerNode() != "remotenode@host" {
		t.Errorf("PeerNode = %q", dc.PeerNode())
	}
	dc.Close()
}

func TestHandshake_WrongCookie(t *testing.T) {
	server, client := net.Pipe()
	go func() {
		serveHandshake(t, server, "correct-cookie", "remote@host")
		server.Close()
	}()

	_, err := Handshake(client, "cnode@host", "wrong-cookie")
	if err == nil {
		t.Error("expected error for wrong cookie")
	}
}

func TestHandshake_BadStatus(t *testing.T) {
	server, client := net.Pipe()
	go func() {
		// Absorb send_name
		readFrame(server)
		// Reply with "not_allowed"
		writeFrame(server, append([]byte{'s'}, "not_allowed"...))
		server.Close()
	}()
	_, err := Handshake(client, "cnode@host", "cookie")
	if err == nil || !strings.Contains(err.Error(), "not_allowed") {
		t.Errorf("expected not_allowed error, got: %v", err)
	}
}

// ── CNode ─────────────────────────────────────────────────────────────────────

func TestCNode_NewCNode(t *testing.T) {
	c := NewCNode("mochi@host", "cookie123")
	if c.Name != "mochi@host" || c.Cookie != "cookie123" {
		t.Errorf("Name=%q Cookie=%q", c.Name, c.Cookie)
	}
}

func TestCNode_Close_NoConns(t *testing.T) {
	c := NewCNode("mochi@host", "cookie")
	if err := c.Close(); err != nil {
		t.Errorf("Close empty: %v", err)
	}
}

func TestCNode_Connect_ThenSendRegSend(t *testing.T) {
	// Spin up a fake Erlang node server.
	server, client := net.Pipe()
	const remoteName = "erlang@localhost"
	const cookie = "testcookie"

	go func() {
		defer server.Close()
		serveHandshake(t, server, cookie, remoteName)
		// Drain any one-way sends from the client (no reply expected).
		io.Copy(io.Discard, server)
	}()

	cnode := NewCNode("mochi@localhost", cookie)
	cnode.Dial = func(network, addr string) (net.Conn, error) {
		return client, nil
	}
	dc, err := cnode.Connect(remoteName, "localhost:12345")
	if err != nil {
		t.Fatalf("Connect: %v", err)
	}

	fromPid := etf.Pid{Node: etf.Atom("mochi@localhost"), ID: 1}
	if err := dc.SendRegSend(fromPid, "my_server", etf.Atom("hello")); err != nil {
		t.Errorf("SendRegSend: %v", err)
	}
	dc.Close()
}

func TestDistConn_SendMsg_FrameFormat(t *testing.T) {
	// Capture raw bytes written by SendRegSend and verify frame structure.
	server, client := net.Pipe()
	defer client.Close()

	var received []byte
	done := make(chan struct{})
	go func() {
		defer close(done)
		var hdr [4]byte
		io.ReadFull(server, hdr[:])
		n := binary.BigEndian.Uint32(hdr[:])
		body := make([]byte, n)
		io.ReadFull(server, body)
		received = body
		server.Close()
	}()

	dc := &DistConn{conn: client, ourNode: "mochi@host", peerNode: "erlang@host"}
	fromPid := etf.Pid{Node: etf.Atom("mochi@host"), ID: 99}
	_ = dc.SendRegSend(fromPid, "my_server", etf.Atom("ping"))

	<-done
	if len(received) == 0 {
		t.Fatal("no bytes received")
	}
	if received[0] != passThroughTag {
		t.Errorf("first byte should be passThroughTag=0x70, got 0x%02x", received[0])
	}
}
