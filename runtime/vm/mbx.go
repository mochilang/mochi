package vm

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"io"
	"math"
	"mochi/interpreter"
	"sort"
)

const (
	mbxArray = '*'
	mbxMap   = '%'
	mbxInt   = ':'
	mbxFloat = ','
	mbxStr   = '$'
	mbxBool  = '#'
	mbxNull  = '_'
)

// MarshalMBX encodes prog into MBX bytes.
func MarshalMBX(prog *Program) ([]byte, error) {
	var buf bytes.Buffer
	if err := encodeProgram(&buf, prog); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

// UnmarshalMBX decodes data into a Program.
func UnmarshalMBX(data []byte) (*Program, error) {
	r := bytes.NewReader(data)
	return decodeProgram(r)
}

func encodeProgram(w *bytes.Buffer, p *Program) error {
	if err := encodeArrayLen(w, len(p.Funcs)); err != nil {
		return err
	}
	for i := range p.Funcs {
		if err := encodeFunction(w, &p.Funcs[i]); err != nil {
			return err
		}
	}
	return nil
}

func decodeProgram(r *bytes.Reader) (*Program, error) {
	n, err := decodeArrayLen(r)
	if err != nil {
		return nil, err
	}
	prog := &Program{Funcs: make([]Function, n)}
	for i := 0; i < n; i++ {
		fn, err := decodeFunction(r)
		if err != nil {
			return nil, err
		}
		prog.Funcs[i] = *fn
	}
	return prog, nil
}

func encodeFunction(w *bytes.Buffer, fn *Function) error {
	if err := encodeArrayLen(w, 4); err != nil {
		return err
	}
	if err := encodeString(w, fn.Name); err != nil {
		return err
	}
	if err := encodeInt(w, fn.NumRegs); err != nil {
		return err
	}
	if err := encodeInt(w, fn.Line); err != nil {
		return err
	}
	if err := encodeArrayLen(w, len(fn.Code)); err != nil {
		return err
	}
	for i := range fn.Code {
		if err := encodeInstr(w, &fn.Code[i]); err != nil {
			return err
		}
	}
	return nil
}

func decodeFunction(r *bytes.Reader) (*Function, error) {
	n, err := decodeArrayLen(r)
	if err != nil {
		return nil, err
	}
	if n != 4 {
		return nil, fmt.Errorf("function array len %d", n)
	}
	name, err := decodeString(r)
	if err != nil {
		return nil, err
	}
	numRegs64, err := decodeInt(r)
	if err != nil {
		return nil, err
	}
	line64, err := decodeInt(r)
	if err != nil {
		return nil, err
	}
	codeLen, err := decodeArrayLen(r)
	if err != nil {
		return nil, err
	}
	fn := &Function{Name: name, NumRegs: int(numRegs64), Line: int(line64), Code: make([]Instr, codeLen)}
	for i := 0; i < codeLen; i++ {
		ins, err := decodeInstr(r)
		if err != nil {
			return nil, err
		}
		fn.Code[i] = *ins
	}
	return fn, nil
}

func encodeInstr(w *bytes.Buffer, ins *Instr) error {
	if err := encodeArrayLen(w, 7); err != nil {
		return err
	}
	if err := encodeInt(w, int(ins.Op)); err != nil {
		return err
	}
	if err := encodeInt(w, ins.A); err != nil {
		return err
	}
	if err := encodeInt(w, ins.B); err != nil {
		return err
	}
	if err := encodeInt(w, ins.C); err != nil {
		return err
	}
	if err := encodeInt(w, ins.D); err != nil {
		return err
	}
	if err := encodeValue(w, ins.Val); err != nil {
		return err
	}
	if err := encodeInt(w, ins.Line); err != nil {
		return err
	}
	return nil
}

func decodeInstr(r *bytes.Reader) (*Instr, error) {
	n, err := decodeArrayLen(r)
	if err != nil {
		return nil, err
	}
	if n != 7 {
		return nil, fmt.Errorf("instr array len %d", n)
	}
	op64, err := decodeInt(r)
	if err != nil {
		return nil, err
	}
	a, err := decodeInt(r)
	if err != nil {
		return nil, err
	}
	b, err := decodeInt(r)
	if err != nil {
		return nil, err
	}
	c, err := decodeInt(r)
	if err != nil {
		return nil, err
	}
	d, err := decodeInt(r)
	if err != nil {
		return nil, err
	}
	val, err := decodeValue(r)
	if err != nil {
		return nil, err
	}
	line64, err := decodeInt(r)
	if err != nil {
		return nil, err
	}
	ins := &Instr{Op: Op(op64), A: int(a), B: int(b), C: int(c), D: int(d), Val: val, Line: int(line64)}
	return ins, nil
}

func encodeValue(w *bytes.Buffer, v Value) error {
	switch v.Tag {
	case interpreter.TagInt:
		w.WriteByte(mbxInt)
		var b [10]byte
		n := binary.PutVarint(b[:], int64(v.Int))
		_, err := w.Write(b[:n])
		return err
	case interpreter.TagFloat:
		w.WriteByte(mbxFloat)
		var b [8]byte
		binary.LittleEndian.PutUint64(b[:], math.Float64bits(v.Float))
		_, err := w.Write(b[:])
		return err
	case interpreter.TagStr:
		if err := encodeString(w, v.Str); err != nil {
			return err
		}
		return nil
	case interpreter.TagBool:
		w.WriteByte(mbxBool)
		if v.Bool {
			return w.WriteByte(1)
		}
		return w.WriteByte(0)
	case interpreter.TagList:
		if err := encodeArrayLen(w, len(v.List)); err != nil {
			return err
		}
		for i := range v.List {
			if err := encodeValue(w, v.List[i]); err != nil {
				return err
			}
		}
		return nil
	case interpreter.TagMap:
		w.WriteByte(mbxMap)
		if err := writeUvarint(w, uint64(len(v.Map))); err != nil {
			return err
		}
		keys := make([]string, 0, len(v.Map))
		for k := range v.Map {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			if err := encodeString(w, k); err != nil {
				return err
			}
			if err := encodeValue(w, v.Map[k]); err != nil {
				return err
			}
		}
		return nil
	case interpreter.TagNull:
		w.WriteByte(mbxNull)
		return nil
	default:
		// treat unknown as null
		w.WriteByte(mbxNull)
		return nil
	}
}

func decodeValue(r *bytes.Reader) (Value, error) {
	prefix, err := r.ReadByte()
	if err != nil {
		return Value{}, err
	}
	switch prefix {
	case mbxNull:
		return Value{}, nil
	case mbxInt:
		n, err := binary.ReadVarint(r)
		if err != nil {
			return Value{}, err
		}
		return Value{Tag: interpreter.TagInt, Int: int(n)}, nil
	case mbxFloat:
		var b [8]byte
		if _, err := io.ReadFull(r, b[:]); err != nil {
			return Value{}, err
		}
		f := math.Float64frombits(binary.LittleEndian.Uint64(b[:]))
		return Value{Tag: interpreter.TagFloat, Float: f}, nil
	case mbxStr:
		s, err := decodeStringWithPrefix(r)
		if err != nil {
			return Value{}, err
		}
		return Value{Tag: interpreter.TagStr, Str: s}, nil
	case mbxBool:
		b, err := r.ReadByte()
		if err != nil {
			return Value{}, err
		}
		return Value{Tag: interpreter.TagBool, Bool: b != 0}, nil
	case mbxArray:
		n, err := binary.ReadUvarint(r)
		if err != nil {
			return Value{}, err
		}
		list := make([]Value, int(n))
		for i := 0; i < int(n); i++ {
			v, err := decodeValue(r)
			if err != nil {
				return Value{}, err
			}
			list[i] = v
		}
		return Value{Tag: interpreter.TagList, List: list}, nil
	case mbxMap:
		n, err := binary.ReadUvarint(r)
		if err != nil {
			return Value{}, err
		}
		m := make(map[string]Value, n)
		for i := 0; i < int(n); i++ {
			key, err := decodeString(r)
			if err != nil {
				return Value{}, err
			}
			v, err := decodeValue(r)
			if err != nil {
				return Value{}, err
			}
			m[key] = v
		}
		return Value{Tag: interpreter.TagMap, Map: m}, nil
	default:
		return Value{}, fmt.Errorf("unknown value prefix %q", prefix)
	}
}

func encodeArrayLen(w *bytes.Buffer, n int) error {
	w.WriteByte(mbxArray)
	return writeUvarint(w, uint64(n))
}

func decodeArrayLen(r *bytes.Reader) (int, error) {
	prefix, err := r.ReadByte()
	if err != nil {
		return 0, err
	}
	if prefix != mbxArray {
		return 0, fmt.Errorf("expected array prefix, got %q", prefix)
	}
	n, err := binary.ReadUvarint(r)
	return int(n), err
}

func encodeString(w *bytes.Buffer, s string) error {
	w.WriteByte(mbxStr)
	if err := writeUvarint(w, uint64(len(s))); err != nil {
		return err
	}
	_, err := w.WriteString(s)
	return err
}

func decodeStringWithPrefix(r *bytes.Reader) (string, error) {
	n, err := binary.ReadUvarint(r)
	if err != nil {
		return "", err
	}
	buf := make([]byte, n)
	if _, err := io.ReadFull(r, buf); err != nil {
		return "", err
	}
	return string(buf), nil
}

func decodeString(r *bytes.Reader) (string, error) {
	prefix, err := r.ReadByte()
	if err != nil {
		return "", err
	}
	if prefix != mbxStr {
		return "", fmt.Errorf("expected string prefix, got %q", prefix)
	}
	return decodeStringWithPrefix(r)
}

func encodeInt(w *bytes.Buffer, i int) error {
	w.WriteByte(mbxInt)
	var b [10]byte
	n := binary.PutVarint(b[:], int64(i))
	_, err := w.Write(b[:n])
	return err
}

func decodeInt(r *bytes.Reader) (int64, error) {
	prefix, err := r.ReadByte()
	if err != nil {
		return 0, err
	}
	if prefix != mbxInt {
		return 0, fmt.Errorf("expected int prefix, got %q", prefix)
	}
	n, err := binary.ReadVarint(r)
	return n, err
}

func writeUvarint(w *bytes.Buffer, u uint64) error {
	var b [10]byte
	n := binary.PutUvarint(b[:], u)
	_, err := w.Write(b[:n])
	return err
}
