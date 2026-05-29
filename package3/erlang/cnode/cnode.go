// Package cnode implements a minimal Erlang C-node in pure Go, allowing
// a Mochi process to participate in the Erlang distribution network without
// starting an erl VM.
//
// # Erlang distribution overview
//
// The Erlang distribution protocol allows multiple nodes to form a cluster.
// Each node registers with the Erlang Port Mapper Daemon (EPMD, port 4369)
// which maps node short-names to TCP port numbers. Nodes connect to each
// other via a TCP handshake, after which they can exchange messages using
// ETF-encoded terms.
//
// # What this package provides
//
// 1. EPMDClient: queries EPMD for a remote node's listen port.
// 2. NodeName: parses and formats "name@host" Erlang node names.
// 3. Handshake (dist_handshake.go): implements the send_name / recv_status /
//    recv_challenge / send_challenge_reply / recv_challenge_ack protocol.
// 4. CNode: connects to a remote node, completes the handshake, and sends
//    ETF-encoded messages.
//
// # Testing without a real Erlang node
//
// All network I/O is abstracted through the Dialer interface so tests can
// inject net.Pipe() connections. The EPMDClient.Dial field works the same way.
package cnode

import (
	"crypto/md5"
	"encoding/binary"
	"fmt"
	"io"
	"math/rand"
	"net"
	"strings"
	"sync"
	"time"

	"mochi/package3/erlang/etf"
)

const (
	// epmdPort is the well-known port for the Erlang Port Mapper Daemon.
	epmdPort = 4369

	// handshakeTimeout is the maximum time allowed for the dist handshake.
	handshakeTimeout = 15 * time.Second

	// distVersion5 is the distribution version flag used in handshake messages.
	distVersion5 = 5

	// Pass-through tag introduced in dist protocol version 6.
	passThroughTag byte = 0x70

	// Control message types.
	ctlREGSEND = 6
)

// NodeName represents a fully qualified Erlang node name ("name@host").
type NodeName struct {
	Name string
	Host string
}

// ParseNodeName splits "name@host" into its parts. Returns an error when the
// format is invalid.
func ParseNodeName(full string) (NodeName, error) {
	idx := strings.LastIndex(full, "@")
	if idx < 1 || idx == len(full)-1 {
		return NodeName{}, fmt.Errorf("cnode: invalid node name %q (expected name@host)", full)
	}
	return NodeName{Name: full[:idx], Host: full[idx+1:]}, nil
}

// String returns the canonical "name@host" representation.
func (n NodeName) String() string { return n.Name + "@" + n.Host }

// Dialer is the factory for TCP connections. Defaults to net.Dial.
type Dialer func(network, addr string) (net.Conn, error)

// EPMDClient queries the Erlang Port Mapper Daemon on a given host.
type EPMDClient struct {
	// Dial is used to open TCP connections. Defaults to net.Dial.
	Dial Dialer
}

// defaultDial wraps net.Dial.
func defaultDial(network, addr string) (net.Conn, error) {
	return net.DialTimeout(network, addr, 5*time.Second)
}

// LookupNode queries EPMD on host for the distribution port of nodeName.
// nodeName is the short name only (without the @host part).
func (c *EPMDClient) LookupNode(host, nodeName string) (int, error) {
	dial := c.Dial
	if dial == nil {
		dial = defaultDial
	}
	conn, err := dial("tcp", fmt.Sprintf("%s:%d", host, epmdPort))
	if err != nil {
		return 0, fmt.Errorf("cnode: EPMD connect: %w", err)
	}
	defer conn.Close()

	// PORT_PLEASE2_REQ = 0x7A
	req := make([]byte, 2+1+len(nodeName))
	binary.BigEndian.PutUint16(req[0:2], uint16(1+len(nodeName)))
	req[2] = 0x7A
	copy(req[3:], nodeName)
	if _, err := conn.Write(req); err != nil {
		return 0, fmt.Errorf("cnode: EPMD write: %w", err)
	}

	// Response: 1 byte tag (0x77) + 1 byte result + rest (if result=0: port info)
	var hdr [2]byte
	if _, err := io.ReadFull(conn, hdr[:]); err != nil {
		return 0, fmt.Errorf("cnode: EPMD read hdr: %w", err)
	}
	if hdr[0] != 0x77 {
		return 0, fmt.Errorf("cnode: EPMD unexpected response tag: 0x%02x", hdr[0])
	}
	if hdr[1] != 0 {
		return 0, fmt.Errorf("cnode: EPMD node %q not found (result=%d)", nodeName, hdr[1])
	}
	// port (2 bytes) + node_type (1) + protocol (1) + highest_version (2) + lowest_version (2) + name_len (2) + name + extra_len (2) + extra
	var portBytes [2]byte
	if _, err := io.ReadFull(conn, portBytes[:]); err != nil {
		return 0, fmt.Errorf("cnode: EPMD read port: %w", err)
	}
	return int(binary.BigEndian.Uint16(portBytes[:])), nil
}

// DistConn is a live connection to a remote Erlang node after the
// distribution handshake has completed.
type DistConn struct {
	conn     net.Conn
	mu       sync.Mutex
	ourNode  string
	peerNode string
}

// SendRegSend sends a message to a registered name on the remote node.
// fromPid is the sender's PID (the C-node's synthetic PID); toName is
// the registered name atom on the remote side. msg is the ETF payload.
func (d *DistConn) SendRegSend(fromPid etf.Pid, toName string, msg interface{}) error {
	// Control: {REG_SEND, FromPid, Cookie='', ToName}
	control := etf.Tuple{
		int64(ctlREGSEND),
		fromPid,
		etf.Atom(""),
		etf.Atom(toName),
	}
	return d.sendMsg(control, msg)
}

// sendMsg encodes and sends a {passthrough, control, msg} dist packet.
func (d *DistConn) sendMsg(control, msg interface{}) error {
	ctlBytes, err := etf.Encode(control)
	if err != nil {
		return fmt.Errorf("cnode: encode control: %w", err)
	}
	msgBytes, err := etf.Encode(msg)
	if err != nil {
		return fmt.Errorf("cnode: encode message: %w", err)
	}
	payload := make([]byte, 1+len(ctlBytes)+len(msgBytes))
	payload[0] = passThroughTag
	copy(payload[1:], ctlBytes)
	copy(payload[1+len(ctlBytes):], msgBytes)

	var hdr [4]byte
	binary.BigEndian.PutUint32(hdr[:], uint32(len(payload)))

	d.mu.Lock()
	defer d.mu.Unlock()
	if _, err := d.conn.Write(hdr[:]); err != nil {
		return err
	}
	_, err = d.conn.Write(payload)
	return err
}

// Close terminates the distribution connection.
func (d *DistConn) Close() error { return d.conn.Close() }

// PeerNode returns the remote node's full name.
func (d *DistConn) PeerNode() string { return d.peerNode }

// CNode represents a Go-side Erlang C-node that can connect to remote
// Erlang nodes via the distribution protocol.
type CNode struct {
	// Name is the full node name, e.g. "mochi_cnode@localhost".
	Name string
	// Cookie is the Erlang magic cookie for authentication.
	Cookie string
	// Dial overrides the TCP dialer. Defaults to net.Dial.
	Dial Dialer

	mu    sync.Mutex
	conns map[string]*DistConn
}

// NewCNode creates a CNode with the given name and cookie.
func NewCNode(name, cookie string) *CNode {
	return &CNode{
		Name:   name,
		Cookie: cookie,
		conns:  make(map[string]*DistConn),
	}
}

// Connect opens a distribution connection to remoteName at addr (host:port).
// addr may be empty, in which case EPMD is queried on the host extracted from
// remoteName to discover the port.
func (c *CNode) Connect(remoteName, addr string) (*DistConn, error) {
	dial := c.Dial
	if dial == nil {
		dial = defaultDial
	}

	if addr == "" {
		nn, err := ParseNodeName(remoteName)
		if err != nil {
			return nil, err
		}
		epmd := &EPMDClient{Dial: dial}
		port, err := epmd.LookupNode(nn.Host, nn.Name)
		if err != nil {
			return nil, err
		}
		addr = fmt.Sprintf("%s:%d", nn.Host, port)
	}

	conn, err := dial("tcp", addr)
	if err != nil {
		return nil, fmt.Errorf("cnode: connect %s: %w", addr, err)
	}

	dc, err := Handshake(conn, c.Name, c.Cookie)
	if err != nil {
		conn.Close()
		return nil, fmt.Errorf("cnode: handshake with %s: %w", remoteName, err)
	}
	dc.peerNode = remoteName

	c.mu.Lock()
	c.conns[remoteName] = dc
	c.mu.Unlock()

	return dc, nil
}

// Close closes all open distribution connections.
func (c *CNode) Close() error {
	c.mu.Lock()
	defer c.mu.Unlock()
	var last error
	for _, dc := range c.conns {
		if err := dc.Close(); err != nil {
			last = err
		}
	}
	c.conns = make(map[string]*DistConn)
	return last
}

// Handshake performs the Erlang distribution handshake over conn and returns
// a DistConn ready for message passing.
//
// The handshake follows the "new" ('N') format introduced in OTP 23:
//
//  1. Client → Server: send_name {'N', flags, creation, name_len, name}
//  2. Server → Client: recv_status {'s', "ok"}
//  3. Server → Client: recv_challenge {'N', flags, challenge, creation, name_len, name}
//  4. Client → Server: send_challenge_reply {'r', our_challenge, digest}
//  5. Server → Client: recv_challenge_ack {'a', digest}
//
// For compatibility with OTP < 23 the 'n' (old) format is also handled in
// recv_challenge.
func Handshake(conn net.Conn, ourName, cookie string) (*DistConn, error) {
	conn.SetDeadline(time.Now().Add(handshakeTimeout))
	defer conn.SetDeadline(time.Time{})

	// Step 1: send_name
	ourChallenge := rand.Uint32()
	if err := sendName(conn, ourName); err != nil {
		return nil, fmt.Errorf("send_name: %w", err)
	}

	// Step 2: recv_status
	status, err := recvStatus(conn)
	if err != nil {
		return nil, fmt.Errorf("recv_status: %w", err)
	}
	if status != "ok" && status != "ok_simultaneous" {
		return nil, fmt.Errorf("recv_status: unexpected status %q", status)
	}

	// Step 3: recv_challenge
	theirChallenge, peerName, err := recvChallenge(conn)
	if err != nil {
		return nil, fmt.Errorf("recv_challenge: %w", err)
	}

	// Step 4: send_challenge_reply
	digest := computeDigest(cookie, theirChallenge)
	if err := sendChallengeReply(conn, ourChallenge, digest); err != nil {
		return nil, fmt.Errorf("send_challenge_reply: %w", err)
	}

	// Step 5: recv_challenge_ack
	expectedAck := computeDigest(cookie, ourChallenge)
	if err := recvChallengeAck(conn, expectedAck); err != nil {
		return nil, fmt.Errorf("recv_challenge_ack: %w", err)
	}

	return &DistConn{conn: conn, ourNode: ourName, peerNode: peerName}, nil
}

// sendName writes the 'N' send_name handshake message.
func sendName(w io.Writer, name string) error {
	// 'N' message: flags(8) + creation(4) + name_len(2) + name
	const flagsMandatory = uint64(0x4000_0000) // DFLAG_HANDSHAKE_23
	nameLen := uint16(len(name))
	body := make([]byte, 1+8+4+2+len(name))
	body[0] = 'N'
	binary.BigEndian.PutUint64(body[1:], flagsMandatory)
	binary.BigEndian.PutUint32(body[9:], 1) // creation
	binary.BigEndian.PutUint16(body[13:], nameLen)
	copy(body[15:], name)
	return writeFrame(w, body)
}

// recvStatus reads the 's' status message and returns the status string.
func recvStatus(r io.Reader) (string, error) {
	body, err := readFrame(r)
	if err != nil {
		return "", err
	}
	if len(body) < 2 || body[0] != 's' {
		return "", fmt.Errorf("expected 's' tag, got 0x%02x", body[0])
	}
	return string(body[1:]), nil
}

// recvChallenge reads the 'n' or 'N' challenge message from the server.
func recvChallenge(r io.Reader) (challenge uint32, peerName string, err error) {
	body, err := readFrame(r)
	if err != nil {
		return 0, "", err
	}
	if len(body) < 1 {
		return 0, "", fmt.Errorf("empty challenge message")
	}
	switch body[0] {
	case 'N': // new format: flags(8) + challenge(4) + creation(4) + name_len(2) + name
		if len(body) < 19 {
			return 0, "", fmt.Errorf("'N' challenge too short: %d", len(body))
		}
		challenge = binary.BigEndian.Uint32(body[9:13])
		// creation occupies body[13:17]; name_len at body[17:19]
		nameLen := binary.BigEndian.Uint16(body[17:19])
		if len(body) < 19+int(nameLen) {
			return 0, "", fmt.Errorf("'N' challenge name truncated")
		}
		peerName = string(body[19 : 19+nameLen])
	case 'n': // old format: version(2) + flags(4) + challenge(4) + name
		if len(body) < 11 {
			return 0, "", fmt.Errorf("'n' challenge too short: %d", len(body))
		}
		challenge = binary.BigEndian.Uint32(body[7:11])
		peerName = string(body[11:])
	default:
		return 0, "", fmt.Errorf("unexpected challenge tag: 0x%02x", body[0])
	}
	return challenge, peerName, nil
}

// sendChallengeReply writes the 'r' message with our challenge and digest.
func sendChallengeReply(w io.Writer, ourChallenge uint32, digest [16]byte) error {
	body := make([]byte, 1+4+16)
	body[0] = 'r'
	binary.BigEndian.PutUint32(body[1:], ourChallenge)
	copy(body[5:], digest[:])
	return writeFrame(w, body)
}

// recvChallengeAck reads the 'a' message and verifies the digest.
func recvChallengeAck(r io.Reader, expected [16]byte) error {
	body, err := readFrame(r)
	if err != nil {
		return err
	}
	if len(body) < 17 || body[0] != 'a' {
		return fmt.Errorf("expected 'a' tag, got 0x%02x (len=%d)", body[0], len(body))
	}
	var got [16]byte
	copy(got[:], body[1:17])
	if got != expected {
		return fmt.Errorf("challenge_ack digest mismatch")
	}
	return nil
}

// computeDigest returns MD5(cookie + challenge_as_big_endian_4_bytes).
func computeDigest(cookie string, challenge uint32) [16]byte {
	var buf [4]byte
	binary.BigEndian.PutUint32(buf[:], challenge)
	h := md5.New()
	h.Write([]byte(cookie))
	h.Write(buf[:])
	var d [16]byte
	copy(d[:], h.Sum(nil))
	return d
}

// writeFrame writes a 2-byte big-endian length followed by body.
func writeFrame(w io.Writer, body []byte) error {
	var hdr [2]byte
	binary.BigEndian.PutUint16(hdr[:], uint16(len(body)))
	if _, err := w.Write(hdr[:]); err != nil {
		return err
	}
	_, err := w.Write(body)
	return err
}

// readFrame reads a 2-byte big-endian length then the body.
func readFrame(r io.Reader) ([]byte, error) {
	var hdr [2]byte
	if _, err := io.ReadFull(r, hdr[:]); err != nil {
		return nil, fmt.Errorf("read frame header: %w", err)
	}
	n := binary.BigEndian.Uint16(hdr[:])
	body := make([]byte, n)
	if _, err := io.ReadFull(r, body); err != nil {
		return nil, fmt.Errorf("read frame body: %w", err)
	}
	return body, nil
}
