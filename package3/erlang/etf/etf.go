// Package etf implements an Erlang External Term Format encoder and decoder
// in pure Go. ETF is the binary serialisation format used by the BEAM virtual
// machine for inter-process messaging, distributed Erlang, and the BEAM file
// abstract-code chunks (Dbgi / Abst) that MEP-66 reads to extract typespecs.
//
// Supported tags (the subset required by MEP-66 phases 0-4):
//
//	70  NEW_FLOAT_EXT         float64
//	97  SMALL_INTEGER_EXT     int64  (0–255)
//	98  INTEGER_EXT           int64  (signed 32-bit)
//	100 ATOM_EXT              Atom   (Latin-1, up to 65535 bytes)
//	104 SMALL_TUPLE_EXT       Tuple  (up to 255 elements)
//	105 LARGE_TUPLE_EXT       Tuple  (up to 2^32 elements)
//	106 NIL_EXT               List{}  (empty list)
//	107 STRING_EXT            List of int64  (Erlang charlist)
//	108 LIST_EXT              List  (proper list with tail)
//	109 BINARY_EXT            []byte
//	110 SMALL_BIG_EXT         int64 / *big.Int
//	111 LARGE_BIG_EXT         *big.Int
//	115 SMALL_ATOM_EXT        Atom  (Latin-1, up to 255 bytes)
//	118 ATOM_UTF8_EXT         Atom  (UTF-8, up to 65535 bytes)
//	119 SMALL_ATOM_UTF8_EXT   Atom  (UTF-8, up to 255 bytes)
//	80  COMPRESSED            zlib-wrapped term
//	88  NEW_PID_EXT           Pid
//	103 PID_EXT               Pid  (legacy)
//	89  NEW_PORT_EXT          ErlPort
//	102 PORT_EXT              ErlPort  (legacy)
//	90  NEWER_REFERENCE_EXT   Reference
//	114 NEW_REFERENCE_EXT     Reference
//	101 REFERENCE_EXT         Reference (legacy)
//
// Unsupported tags return ErrUnsupportedTag.
package etf

import (
	"bytes"
	"compress/zlib"
	"encoding/binary"
	"fmt"
	"io"
	"math"
	"math/big"
)

// versionMagic is the first byte of every top-level ETF term.
const versionMagic byte = 131

// ETF tag bytes.
const (
	tagCompressed     byte = 80
	tagNewFloat       byte = 70
	tagSmallInteger   byte = 97
	tagInteger        byte = 98
	tagAtom           byte = 100 // latin-1, length uint16
	tagPid            byte = 103 // legacy
	tagSmallTuple     byte = 104
	tagLargeTuple     byte = 105
	tagNil            byte = 106
	tagString         byte = 107 // charlist compact form
	tagList           byte = 108
	tagBinary         byte = 109
	tagSmallBig       byte = 110
	tagLargeBig       byte = 111
	tagReference      byte = 101 // legacy
	tagNewReference   byte = 114
	tagSmallAtom      byte = 115 // latin-1, length uint8
	tagAtomUTF8       byte = 118 // utf-8, length uint16
	tagSmallAtomUTF8  byte = 119 // utf-8, length uint8
	tagPort           byte = 102 // legacy
	tagNewPid         byte = 88
	tagNewPort        byte = 89
	tagNewerReference byte = 90
)

// ErrUnsupportedTag is returned when Decode encounters a tag byte the bridge
// does not need to handle.
type ErrUnsupportedTag struct {
	Tag byte
	At  int64
}

func (e *ErrUnsupportedTag) Error() string {
	return fmt.Sprintf("etf: unsupported tag 0x%02x (%d) at byte offset %d", e.Tag, e.Tag, e.At)
}

// Atom is an Erlang atom represented as a Go string.
type Atom string

// Tuple is an Erlang tuple.
type Tuple []interface{}

// List is an Erlang proper list.
type List []interface{}

// Pid is an Erlang process identifier. Treated as an opaque handle by the
// bridge; it is passed through ETF encoding/decoding without interpretation.
type Pid struct {
	Node     Atom
	ID       uint32
	Serial   uint32
	Creation uint32
}

// ErlPort is an Erlang port identifier. Opaque in the bridge.
type ErlPort struct {
	Node     Atom
	ID       uint32
	Creation uint32
}

// Reference is an Erlang reference term. Opaque in the bridge.
type Reference struct {
	Node     Atom
	Creation uint32
	IDs      []uint32
}

// Decode decodes a single ETF term from data. data must begin with the
// version-magic byte (131). Returns the decoded Go value.
func Decode(data []byte) (interface{}, error) {
	if len(data) < 2 {
		return nil, fmt.Errorf("etf: input too short (%d bytes)", len(data))
	}
	if data[0] != versionMagic {
		return nil, fmt.Errorf("etf: expected version magic 131, got %d", data[0])
	}
	d := &decoder{buf: data, pos: 1}
	return d.term()
}

// decoder is a read cursor over an ETF byte slice.
type decoder struct {
	buf []byte
	pos int
}

func (d *decoder) byte_() (byte, error) {
	if d.pos >= len(d.buf) {
		return 0, io.ErrUnexpectedEOF
	}
	b := d.buf[d.pos]
	d.pos++
	return b, nil
}

func (d *decoder) bytes_(n int) ([]byte, error) {
	if d.pos+n > len(d.buf) {
		return nil, io.ErrUnexpectedEOF
	}
	b := d.buf[d.pos : d.pos+n]
	d.pos += n
	return b, nil
}

func (d *decoder) uint16() (uint16, error) {
	b, err := d.bytes_(2)
	if err != nil {
		return 0, err
	}
	return binary.BigEndian.Uint16(b), nil
}

func (d *decoder) uint32() (uint32, error) {
	b, err := d.bytes_(4)
	if err != nil {
		return 0, err
	}
	return binary.BigEndian.Uint32(b), nil
}

func (d *decoder) term() (interface{}, error) {
	tag, err := d.byte_()
	if err != nil {
		return nil, fmt.Errorf("etf: read tag: %w", err)
	}
	switch tag {
	case tagCompressed:
		return d.compressed()
	case tagNewFloat:
		return d.newFloat()
	case tagSmallInteger:
		b, err := d.byte_()
		if err != nil {
			return nil, err
		}
		return int64(b), nil
	case tagInteger:
		b, err := d.bytes_(4)
		if err != nil {
			return nil, err
		}
		return int64(int32(binary.BigEndian.Uint32(b))), nil
	case tagAtom:
		return d.atomLatin1(false)
	case tagSmallAtom:
		return d.atomLatin1(true)
	case tagAtomUTF8:
		return d.atomUTF8(false)
	case tagSmallAtomUTF8:
		return d.atomUTF8(true)
	case tagSmallTuple:
		ar, err := d.byte_()
		if err != nil {
			return nil, err
		}
		return d.tupleElems(int(ar))
	case tagLargeTuple:
		ar, err := d.uint32()
		if err != nil {
			return nil, err
		}
		return d.tupleElems(int(ar))
	case tagNil:
		return List(nil), nil
	case tagString:
		return d.stringExt()
	case tagList:
		return d.list()
	case tagBinary:
		return d.binary()
	case tagSmallBig:
		n, err := d.byte_()
		if err != nil {
			return nil, err
		}
		return d.bigInt(int(n))
	case tagLargeBig:
		n, err := d.uint32()
		if err != nil {
			return nil, err
		}
		return d.bigInt(int(n))
	case tagPid:
		return d.pid(false)
	case tagNewPid:
		return d.pid(true)
	case tagPort:
		return d.port(false)
	case tagNewPort:
		return d.port(true)
	case tagReference:
		return d.referenceOld()
	case tagNewReference:
		return d.newReference(false)
	case tagNewerReference:
		return d.newReference(true)
	default:
		return nil, &ErrUnsupportedTag{Tag: tag, At: int64(d.pos - 1)}
	}
}

func (d *decoder) compressed() (interface{}, error) {
	uncompLen, err := d.uint32()
	if err != nil {
		return nil, fmt.Errorf("etf: compressed: uncompressed length: %w", err)
	}
	// All remaining bytes are the zlib-compressed payload.
	compressed := d.buf[d.pos:]
	d.pos = len(d.buf)
	zr, err := zlib.NewReader(bytes.NewReader(compressed))
	if err != nil {
		return nil, fmt.Errorf("etf: compressed: zlib.NewReader: %w", err)
	}
	defer zr.Close()
	plain := make([]byte, uncompLen)
	if _, err := io.ReadFull(zr, plain); err != nil {
		return nil, fmt.Errorf("etf: compressed: decompress: %w", err)
	}
	inner := &decoder{buf: plain, pos: 0}
	return inner.term()
}

func (d *decoder) newFloat() (interface{}, error) {
	b, err := d.bytes_(8)
	if err != nil {
		return nil, fmt.Errorf("etf: new_float: %w", err)
	}
	bits := binary.BigEndian.Uint64(b)
	return math.Float64frombits(bits), nil
}

func (d *decoder) atomLatin1(small bool) (Atom, error) {
	var length int
	if small {
		b, err := d.byte_()
		if err != nil {
			return "", err
		}
		length = int(b)
	} else {
		n, err := d.uint16()
		if err != nil {
			return "", err
		}
		length = int(n)
	}
	b, err := d.bytes_(length)
	if err != nil {
		return "", fmt.Errorf("etf: atom_latin1: %w", err)
	}
	// Latin-1 codepoints 0-127 are identical to ASCII/UTF-8; 128-255 map to
	// U+0080-U+00FF (Latin-1 supplement) which are two-byte sequences in UTF-8.
	runes := make([]rune, length)
	for i, c := range b {
		runes[i] = rune(c)
	}
	return Atom(string(runes)), nil
}

func (d *decoder) atomUTF8(small bool) (Atom, error) {
	var length int
	if small {
		b, err := d.byte_()
		if err != nil {
			return "", err
		}
		length = int(b)
	} else {
		n, err := d.uint16()
		if err != nil {
			return "", err
		}
		length = int(n)
	}
	b, err := d.bytes_(length)
	if err != nil {
		return "", fmt.Errorf("etf: atom_utf8: %w", err)
	}
	return Atom(string(b)), nil
}

func (d *decoder) tupleElems(n int) (Tuple, error) {
	elems := make(Tuple, n)
	for i := 0; i < n; i++ {
		v, err := d.term()
		if err != nil {
			return nil, fmt.Errorf("etf: tuple[%d]: %w", i, err)
		}
		elems[i] = v
	}
	return elems, nil
}

func (d *decoder) stringExt() (List, error) {
	length, err := d.uint16()
	if err != nil {
		return nil, err
	}
	b, err := d.bytes_(int(length))
	if err != nil {
		return nil, fmt.Errorf("etf: string_ext: %w", err)
	}
	// STRING_EXT is a compact charlist: list of character codepoints.
	elems := make(List, len(b))
	for i, c := range b {
		elems[i] = int64(c)
	}
	return elems, nil
}

func (d *decoder) list() (List, error) {
	length, err := d.uint32()
	if err != nil {
		return nil, err
	}
	elems := make(List, int(length))
	for i := range elems {
		v, err := d.term()
		if err != nil {
			return nil, fmt.Errorf("etf: list[%d]: %w", i, err)
		}
		elems[i] = v
	}
	// Consume the tail term (NIL for a proper list).
	tail, err := d.term()
	if err != nil {
		return nil, fmt.Errorf("etf: list tail: %w", err)
	}
	if tv, ok := tail.(List); ok && len(tv) > 0 {
		elems = append(elems, tv...)
	} else if _, ok := tail.(List); !ok {
		// Improper list: append tail as last element.
		elems = append(elems, tail)
	}
	return elems, nil
}

func (d *decoder) binary() ([]byte, error) {
	length, err := d.uint32()
	if err != nil {
		return nil, err
	}
	b, err := d.bytes_(int(length))
	if err != nil {
		return nil, fmt.Errorf("etf: binary: %w", err)
	}
	out := make([]byte, length)
	copy(out, b)
	return out, nil
}

func (d *decoder) bigInt(n int) (interface{}, error) {
	sign, err := d.byte_()
	if err != nil {
		return nil, err
	}
	digits, err := d.bytes_(n)
	if err != nil {
		return nil, fmt.Errorf("etf: big_ext: digits: %w", err)
	}
	// Digits are little-endian; reverse for big.Int which is big-endian.
	rev := make([]byte, n)
	for i, b := range digits {
		rev[n-1-i] = b
	}
	bi := new(big.Int).SetBytes(rev)
	if sign != 0 {
		bi.Neg(bi)
	}
	if bi.IsInt64() {
		return bi.Int64(), nil
	}
	return bi, nil
}

func (d *decoder) pid(isNew bool) (Pid, error) {
	node, err := d.term()
	if err != nil {
		return Pid{}, fmt.Errorf("etf: pid node: %w", err)
	}
	na, ok := node.(Atom)
	if !ok {
		return Pid{}, fmt.Errorf("etf: pid node not atom, got %T", node)
	}
	id, err := d.uint32()
	if err != nil {
		return Pid{}, err
	}
	serial, err := d.uint32()
	if err != nil {
		return Pid{}, err
	}
	var creation uint32
	if isNew {
		creation, err = d.uint32()
	} else {
		var b byte
		b, err = d.byte_()
		creation = uint32(b)
	}
	if err != nil {
		return Pid{}, fmt.Errorf("etf: pid creation: %w", err)
	}
	return Pid{Node: na, ID: id, Serial: serial, Creation: creation}, nil
}

func (d *decoder) port(isNew bool) (ErlPort, error) {
	node, err := d.term()
	if err != nil {
		return ErlPort{}, fmt.Errorf("etf: port node: %w", err)
	}
	na, ok := node.(Atom)
	if !ok {
		return ErlPort{}, fmt.Errorf("etf: port node not atom")
	}
	id, err := d.uint32()
	if err != nil {
		return ErlPort{}, err
	}
	var creation uint32
	if isNew {
		creation, err = d.uint32()
	} else {
		var b byte
		b, err = d.byte_()
		creation = uint32(b)
	}
	if err != nil {
		return ErlPort{}, fmt.Errorf("etf: port creation: %w", err)
	}
	return ErlPort{Node: na, ID: id, Creation: creation}, nil
}

func (d *decoder) referenceOld() (Reference, error) {
	node, err := d.term()
	if err != nil {
		return Reference{}, err
	}
	na, ok := node.(Atom)
	if !ok {
		return Reference{}, fmt.Errorf("etf: reference node not atom")
	}
	id, err := d.uint32()
	if err != nil {
		return Reference{}, err
	}
	creation, err := d.byte_()
	if err != nil {
		return Reference{}, err
	}
	return Reference{Node: na, Creation: uint32(creation), IDs: []uint32{id}}, nil
}

func (d *decoder) newReference(newer bool) (Reference, error) {
	length, err := d.uint16()
	if err != nil {
		return Reference{}, err
	}
	node, err := d.term()
	if err != nil {
		return Reference{}, err
	}
	na, ok := node.(Atom)
	if !ok {
		return Reference{}, fmt.Errorf("etf: reference node not atom")
	}
	var creation uint32
	if newer {
		creation, err = d.uint32()
	} else {
		var b byte
		b, err = d.byte_()
		creation = uint32(b)
	}
	if err != nil {
		return Reference{}, fmt.Errorf("etf: reference creation: %w", err)
	}
	ids := make([]uint32, int(length))
	for i := range ids {
		ids[i], err = d.uint32()
		if err != nil {
			return Reference{}, fmt.Errorf("etf: reference id[%d]: %w", i, err)
		}
	}
	return Reference{Node: na, Creation: creation, IDs: ids}, nil
}

// ---- Encoder ----------------------------------------------------------------

// Encode encodes v as an ETF term prefixed with the version-magic byte.
func Encode(v interface{}) ([]byte, error) {
	var buf bytes.Buffer
	buf.WriteByte(versionMagic)
	if err := encTerm(&buf, v); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

func encTerm(buf *bytes.Buffer, v interface{}) error {
	switch val := v.(type) {
	case bool:
		if val {
			return encAtom(buf, Atom("true"))
		}
		return encAtom(buf, Atom("false"))
	case int:
		return encInt64(buf, int64(val))
	case int64:
		return encInt64(buf, val)
	case float64:
		buf.WriteByte(tagNewFloat)
		var b [8]byte
		binary.BigEndian.PutUint64(b[:], math.Float64bits(val))
		buf.Write(b[:])
		return nil
	case Atom:
		return encAtom(buf, val)
	case string:
		return encBinary(buf, []byte(val))
	case []byte:
		return encBinary(buf, val)
	case Tuple:
		return encTuple(buf, val)
	case List:
		return encList(buf, val)
	case Pid:
		return encPid(buf, val)
	case ErlPort:
		return encPort(buf, val)
	case Reference:
		return encReference(buf, val)
	case nil:
		buf.WriteByte(tagNil)
		return nil
	default:
		return fmt.Errorf("etf: Encode: unsupported type %T", v)
	}
}

func encInt64(buf *bytes.Buffer, v int64) error {
	switch {
	case v >= 0 && v <= 255:
		buf.WriteByte(tagSmallInteger)
		buf.WriteByte(byte(v))
	case v >= -2147483648 && v <= 2147483647:
		buf.WriteByte(tagInteger)
		var b [4]byte
		binary.BigEndian.PutUint32(b[:], uint32(int32(v)))
		buf.Write(b[:])
	default:
		// SMALL_BIG_EXT for values outside 32-bit range.
		sign := byte(0)
		abs := uint64(v)
		if v < 0 {
			sign = 1
			abs = uint64(-v)
		}
		var digits []byte
		for abs > 0 {
			digits = append(digits, byte(abs&0xff))
			abs >>= 8
		}
		if len(digits) > 255 {
			return fmt.Errorf("etf: int64 too large for SMALL_BIG_EXT")
		}
		buf.WriteByte(tagSmallBig)
		buf.WriteByte(byte(len(digits)))
		buf.WriteByte(sign)
		buf.Write(digits)
	}
	return nil
}

func encAtom(buf *bytes.Buffer, a Atom) error {
	b := []byte(string(a))
	if len(b) > 65535 {
		return fmt.Errorf("etf: atom too long (%d bytes)", len(b))
	}
	if len(b) <= 255 {
		buf.WriteByte(tagSmallAtomUTF8)
		buf.WriteByte(byte(len(b)))
	} else {
		buf.WriteByte(tagAtomUTF8)
		var l [2]byte
		binary.BigEndian.PutUint16(l[:], uint16(len(b)))
		buf.Write(l[:])
	}
	buf.Write(b)
	return nil
}

func encBinary(buf *bytes.Buffer, b []byte) error {
	buf.WriteByte(tagBinary)
	var l [4]byte
	binary.BigEndian.PutUint32(l[:], uint32(len(b)))
	buf.Write(l[:])
	buf.Write(b)
	return nil
}

func encTuple(buf *bytes.Buffer, t Tuple) error {
	if len(t) <= 255 {
		buf.WriteByte(tagSmallTuple)
		buf.WriteByte(byte(len(t)))
	} else {
		buf.WriteByte(tagLargeTuple)
		var l [4]byte
		binary.BigEndian.PutUint32(l[:], uint32(len(t)))
		buf.Write(l[:])
	}
	for i, elem := range t {
		if err := encTerm(buf, elem); err != nil {
			return fmt.Errorf("etf: tuple[%d]: %w", i, err)
		}
	}
	return nil
}

func encList(buf *bytes.Buffer, l List) error {
	if len(l) == 0 {
		buf.WriteByte(tagNil)
		return nil
	}
	buf.WriteByte(tagList)
	var length [4]byte
	binary.BigEndian.PutUint32(length[:], uint32(len(l)))
	buf.Write(length[:])
	for i, elem := range l {
		if err := encTerm(buf, elem); err != nil {
			return fmt.Errorf("etf: list[%d]: %w", i, err)
		}
	}
	buf.WriteByte(tagNil) // proper list tail
	return nil
}

func encPid(buf *bytes.Buffer, p Pid) error {
	buf.WriteByte(tagNewPid)
	if err := encAtom(buf, p.Node); err != nil {
		return err
	}
	var b [12]byte
	binary.BigEndian.PutUint32(b[0:4], p.ID)
	binary.BigEndian.PutUint32(b[4:8], p.Serial)
	binary.BigEndian.PutUint32(b[8:12], p.Creation)
	buf.Write(b[:])
	return nil
}

func encPort(buf *bytes.Buffer, p ErlPort) error {
	buf.WriteByte(tagNewPort)
	if err := encAtom(buf, p.Node); err != nil {
		return err
	}
	var b [8]byte
	binary.BigEndian.PutUint32(b[0:4], p.ID)
	binary.BigEndian.PutUint32(b[4:8], p.Creation)
	buf.Write(b[:])
	return nil
}

func encReference(buf *bytes.Buffer, r Reference) error {
	buf.WriteByte(tagNewerReference)
	var l [2]byte
	binary.BigEndian.PutUint16(l[:], uint16(len(r.IDs)))
	buf.Write(l[:])
	if err := encAtom(buf, r.Node); err != nil {
		return err
	}
	var c [4]byte
	binary.BigEndian.PutUint32(c[:], r.Creation)
	buf.Write(c[:])
	for _, id := range r.IDs {
		var b [4]byte
		binary.BigEndian.PutUint32(b[:], id)
		buf.Write(b[:])
	}
	return nil
}
