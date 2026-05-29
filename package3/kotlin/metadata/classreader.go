// Package metadata extracts the Kotlin API surface from @kotlin.Metadata annotations
// inside JAR class files without spawning a JVM.
package metadata

import (
	"encoding/binary"
	"fmt"

	bridgeerrors "github.com/mochilang/mochi/package3/kotlin/errors"
)

// RawMetadata holds the raw contents of a @kotlin.Metadata annotation.
type RawMetadata struct {
	Kind    int32
	Version [3]int32
	D1      []byte   // joined d1 strings (raw proto bytes)
	D2      []string // string table
	XS      string
	XI      int32
}

// constantPool holds the constant pool entries we need for annotation parsing.
type constantPool struct {
	utf8    map[int]string
	integer map[int]int32
}

// ExtractMetadata reads a JVM .class file (classBytes) and extracts the
// @kotlin.Metadata annotation if present. Returns ErrNoKotlinMetadata if the
// annotation is absent. Malformed class data returns a descriptive error.
func ExtractMetadata(classBytes []byte) (*RawMetadata, error) {
	r := &classReader{data: classBytes}

	// Check magic 0xCAFEBABE
	magic, err := r.readU32()
	if err != nil {
		return nil, fmt.Errorf("classreader: cannot read magic: %w", err)
	}
	if magic != 0xCAFEBABE {
		return nil, fmt.Errorf("classreader: invalid class file magic: %#x", magic)
	}

	// Skip minor_version and major_version (4 bytes total)
	if err := r.skip(4); err != nil {
		return nil, fmt.Errorf("classreader: cannot skip version: %w", err)
	}

	// Read constant pool
	cpCount, err := r.readU16()
	if err != nil {
		return nil, fmt.Errorf("classreader: cannot read constant pool count: %w", err)
	}

	cp := &constantPool{
		utf8:    make(map[int]string),
		integer: make(map[int]int32),
	}

	// Parse constant_pool[1 .. cpCount-1]
	i := 1
	for i < int(cpCount) {
		tag, err := r.readU8()
		if err != nil {
			return nil, fmt.Errorf("classreader: cannot read cp tag at index %d: %w", i, err)
		}
		switch tag {
		case 1: // Utf8
			length, err := r.readU16()
			if err != nil {
				return nil, fmt.Errorf("classreader: Utf8 length at cp[%d]: %w", i, err)
			}
			bs, err := r.readN(int(length))
			if err != nil {
				return nil, fmt.Errorf("classreader: Utf8 bytes at cp[%d]: %w", i, err)
			}
			cp.utf8[i] = string(bs)
		case 3: // Integer
			bs, err := r.readN(4)
			if err != nil {
				return nil, fmt.Errorf("classreader: Integer bytes at cp[%d]: %w", i, err)
			}
			cp.integer[i] = int32(binary.BigEndian.Uint32(bs))
		case 4: // Float
			if err := r.skip(4); err != nil {
				return nil, fmt.Errorf("classreader: skip Float cp[%d]: %w", i, err)
			}
		case 5, 6: // Long, Double — 8 bytes, next index unusable
			if err := r.skip(8); err != nil {
				return nil, fmt.Errorf("classreader: skip Long/Double cp[%d]: %w", i, err)
			}
			i++ // skip next slot
		case 7, 8, 16, 19, 20: // Class, String, MethodType, Module, Package — 2 bytes
			if err := r.skip(2); err != nil {
				return nil, fmt.Errorf("classreader: skip 2-byte cp[%d] tag=%d: %w", i, tag, err)
			}
		case 9, 10, 11, 12, 17, 18: // Fieldref, Methodref, InterfaceMethodref, NameAndType, Dynamic, InvokeDynamic — 4 bytes
			if err := r.skip(4); err != nil {
				return nil, fmt.Errorf("classreader: skip 4-byte cp[%d] tag=%d: %w", i, tag, err)
			}
		case 15: // MethodHandle — 3 bytes
			if err := r.skip(3); err != nil {
				return nil, fmt.Errorf("classreader: skip MethodHandle cp[%d]: %w", i, err)
			}
		default:
			return nil, fmt.Errorf("classreader: unknown constant pool tag %d at index %d", tag, i)
		}
		i++
	}

	// Skip access_flags (2), this_class (2), super_class (2)
	if err := r.skip(6); err != nil {
		return nil, fmt.Errorf("classreader: skip access/this/super: %w", err)
	}

	// Skip interfaces
	ifCount, err := r.readU16()
	if err != nil {
		return nil, fmt.Errorf("classreader: read interfaces count: %w", err)
	}
	if err := r.skip(int(ifCount) * 2); err != nil {
		return nil, fmt.Errorf("classreader: skip interfaces: %w", err)
	}

	// Skip fields
	if err := skipMembersTable(r); err != nil {
		return nil, fmt.Errorf("classreader: skip fields: %w", err)
	}

	// Skip methods
	if err := skipMembersTable(r); err != nil {
		return nil, fmt.Errorf("classreader: skip methods: %w", err)
	}

	// Read class attributes
	attrCount, err := r.readU16()
	if err != nil {
		return nil, fmt.Errorf("classreader: read class attributes count: %w", err)
	}

	for a := 0; a < int(attrCount); a++ {
		nameIdx, err := r.readU16()
		if err != nil {
			return nil, fmt.Errorf("classreader: read attr name_index: %w", err)
		}
		attrLen, err := r.readU32()
		if err != nil {
			return nil, fmt.Errorf("classreader: read attr length: %w", err)
		}
		attrData, err := r.readN(int(attrLen))
		if err != nil {
			return nil, fmt.Errorf("classreader: read attr data: %w", err)
		}

		attrName := cp.utf8[int(nameIdx)]
		if attrName == "RuntimeVisibleAnnotations" {
			meta, err := parseAnnotations(attrData, cp)
			if err != nil {
				return nil, fmt.Errorf("classreader: parse RuntimeVisibleAnnotations: %w", err)
			}
			if meta != nil {
				return meta, nil
			}
		}
	}

	return nil, bridgeerrors.ErrNoKotlinMetadata
}

// skipMembersTable skips the fields or methods table in a class file.
func skipMembersTable(r *classReader) error {
	count, err := r.readU16()
	if err != nil {
		return err
	}
	for i := 0; i < int(count); i++ {
		// access_flags, name_index, descriptor_index = 6 bytes
		if err := r.skip(6); err != nil {
			return err
		}
		if err := skipAttributes(r); err != nil {
			return err
		}
	}
	return nil
}

// skipAttributes skips an attributes table.
func skipAttributes(r *classReader) error {
	count, err := r.readU16()
	if err != nil {
		return err
	}
	for i := 0; i < int(count); i++ {
		// name_index (2) + length (4) + data
		if err := r.skip(2); err != nil {
			return err
		}
		length, err := r.readU32()
		if err != nil {
			return err
		}
		if err := r.skip(int(length)); err != nil {
			return err
		}
	}
	return nil
}

// parseAnnotations parses a RuntimeVisibleAnnotations attribute and extracts
// the @kotlin.Metadata annotation if present. Returns nil if not found.
func parseAnnotations(data []byte, cp *constantPool) (*RawMetadata, error) {
	r := &classReader{data: data}

	numAnnotations, err := r.readU16()
	if err != nil {
		return nil, fmt.Errorf("read num_annotations: %w", err)
	}

	for i := 0; i < int(numAnnotations); i++ {
		typeIdx, err := r.readU16()
		if err != nil {
			return nil, fmt.Errorf("read annotation type_index: %w", err)
		}
		numPairs, err := r.readU16()
		if err != nil {
			return nil, fmt.Errorf("read num_element_value_pairs: %w", err)
		}

		typeName := cp.utf8[int(typeIdx)]
		isKotlinMeta := typeName == "Lkotlin/Metadata;"

		meta := &RawMetadata{}
		for p := 0; p < int(numPairs); p++ {
			elemNameIdx, err := r.readU16()
			if err != nil {
				return nil, fmt.Errorf("read element name_index: %w", err)
			}
			elemName := cp.utf8[int(elemNameIdx)]

			val, err := parseElementValue(r, cp)
			if err != nil {
				return nil, fmt.Errorf("parse element value for %q: %w", elemName, err)
			}

			if isKotlinMeta {
				applyMetaField(meta, elemName, val)
			}
		}

		if isKotlinMeta {
			return meta, nil
		}
	}
	return nil, nil
}

// applyMetaField sets the appropriate field on meta based on the element name.
func applyMetaField(meta *RawMetadata, name string, val interface{}) {
	switch name {
	case "k":
		if v, ok := val.(int32); ok {
			meta.Kind = v
		}
	case "xi":
		if v, ok := val.(int32); ok {
			meta.XI = v
		}
	case "mv":
		if arr, ok := val.([]interface{}); ok {
			for i, vv := range arr {
				if i >= 3 {
					break
				}
				if v, ok := vv.(int32); ok {
					meta.Version[i] = v
				}
			}
		}
	case "d1":
		if arr, ok := val.([]interface{}); ok {
			var buf []byte
			for _, vv := range arr {
				if s, ok := vv.(string); ok {
					buf = append(buf, []byte(s)...)
				}
			}
			meta.D1 = buf
		}
	case "d2":
		if arr, ok := val.([]interface{}); ok {
			for _, vv := range arr {
				if s, ok := vv.(string); ok {
					meta.D2 = append(meta.D2, s)
				}
			}
		}
	case "xs":
		if s, ok := val.(string); ok {
			meta.XS = s
		}
	}
}

// parseElementValue reads one element_value and returns a Go value:
//   - int32 for tags I (and B,C,D,F,J,S,Z treated as int32 via constant pool)
//   - string for tag 's'
//   - []interface{} for tag '['
//   - nil for tags 'e', 'c', '@'
func parseElementValue(r *classReader, cp *constantPool) (interface{}, error) {
	tag, err := r.readU8()
	if err != nil {
		return nil, fmt.Errorf("read element value tag: %w", err)
	}

	switch tag {
	case 'B', 'C', 'F', 'S', 'Z': // primitive (not commonly used in kotlin.Metadata)
		constIdx, err := r.readU16()
		if err != nil {
			return nil, fmt.Errorf("read primitive const_value_index: %w", err)
		}
		// Resolve as int32 from integer pool; for B/C/S/Z these would be Integer constants too
		return cp.integer[int(constIdx)], nil
	case 'I': // Integer
		constIdx, err := r.readU16()
		if err != nil {
			return nil, fmt.Errorf("read Integer const_value_index: %w", err)
		}
		return cp.integer[int(constIdx)], nil
	case 'J': // Long
		constIdx, err := r.readU16()
		if err != nil {
			return nil, fmt.Errorf("read Long const_value_index: %w", err)
		}
		// Long is stored but not used by kotlin.Metadata; return as int32 for compat
		return cp.integer[int(constIdx)], nil
	case 'D': // Double — skip
		if err := r.skip(2); err != nil {
			return nil, fmt.Errorf("skip Double const_value_index: %w", err)
		}
		return nil, nil
	case 'e': // enum: type_name_index, const_name_index
		if err := r.skip(4); err != nil {
			return nil, fmt.Errorf("skip enum element: %w", err)
		}
		return nil, nil
	case 'c': // class: class_info_index
		if err := r.skip(2); err != nil {
			return nil, fmt.Errorf("skip class element: %w", err)
		}
		return nil, nil
	case 's': // string: const_value_index -> Utf8
		constIdx, err := r.readU16()
		if err != nil {
			return nil, fmt.Errorf("read string const_value_index: %w", err)
		}
		return cp.utf8[int(constIdx)], nil
	case '@': // nested annotation
		if err := r.skip(2); err != nil { // type_index
			return nil, err
		}
		numPairs, err := r.readU16()
		if err != nil {
			return nil, err
		}
		for i := 0; i < int(numPairs); i++ {
			if err := r.skip(2); err != nil { // name_index
				return nil, err
			}
			if _, err := parseElementValue(r, cp); err != nil {
				return nil, err
			}
		}
		return nil, nil
	case '[': // array
		numVals, err := r.readU16()
		if err != nil {
			return nil, fmt.Errorf("read array num_values: %w", err)
		}
		arr := make([]interface{}, 0, int(numVals))
		for i := 0; i < int(numVals); i++ {
			v, err := parseElementValue(r, cp)
			if err != nil {
				return nil, fmt.Errorf("array element %d: %w", i, err)
			}
			arr = append(arr, v)
		}
		return arr, nil
	default:
		return nil, fmt.Errorf("unknown element value tag: %c (%d)", tag, tag)
	}
}

// classReader is a simple forward-only big-endian byte reader.
type classReader struct {
	data []byte
	pos  int
}

func (r *classReader) readU8() (byte, error) {
	if r.pos >= len(r.data) {
		return 0, fmt.Errorf("unexpected EOF at offset %d", r.pos)
	}
	b := r.data[r.pos]
	r.pos++
	return b, nil
}

func (r *classReader) readU16() (uint16, error) {
	if r.pos+2 > len(r.data) {
		return 0, fmt.Errorf("unexpected EOF at offset %d (need 2 bytes)", r.pos)
	}
	v := binary.BigEndian.Uint16(r.data[r.pos:])
	r.pos += 2
	return v, nil
}

func (r *classReader) readU32() (uint32, error) {
	if r.pos+4 > len(r.data) {
		return 0, fmt.Errorf("unexpected EOF at offset %d (need 4 bytes)", r.pos)
	}
	v := binary.BigEndian.Uint32(r.data[r.pos:])
	r.pos += 4
	return v, nil
}

func (r *classReader) readN(n int) ([]byte, error) {
	if r.pos+n > len(r.data) {
		return nil, fmt.Errorf("unexpected EOF at offset %d (need %d bytes)", r.pos, n)
	}
	b := r.data[r.pos : r.pos+n]
	r.pos += n
	return b, nil
}

func (r *classReader) skip(n int) error {
	if r.pos+n > len(r.data) {
		return fmt.Errorf("unexpected EOF at offset %d (skip %d bytes)", r.pos, n)
	}
	r.pos += n
	return nil
}
