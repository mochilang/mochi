package bmon

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"reflect"
	"sort"
	"strconv"
	"time"
)

// Marshal encodes v into BMON bytes.
func Marshal(v any) ([]byte, error) {
	var buf bytes.Buffer
	if err := NewEncoder(&buf).Encode(v); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

// Unmarshal parses a BMON value from data into v.
// v must be a non-nil pointer.
func Unmarshal(data []byte, v any) error {
	return NewDecoder(bytes.NewReader(data)).Decode(v)
}

// Encoder writes BMON values to an io.Writer.
type Encoder struct {
	w *bufio.Writer
}

// NewEncoder returns a new Encoder.
func NewEncoder(w io.Writer) *Encoder {
	return &Encoder{w: bufio.NewWriter(w)}
}

// Encode writes v to the underlying writer.
func (e *Encoder) Encode(v any) error {
	if err := encodeValue(e.w, v); err != nil {
		return err
	}
	return e.w.Flush()
}

func encodeValue(w *bufio.Writer, v any) error {
	switch val := v.(type) {
	case nil:
		_, err := w.WriteString("_\r\n")
		return err
	case string:
		if _, err := w.WriteString("+" + val + "\r\n"); err != nil {
			return err
		}
		return nil
	case error:
		if _, err := w.WriteString("-" + val.Error() + "\r\n"); err != nil {
			return err
		}
		return nil
	case int:
		return encodeValue(w, int64(val))
	case int64:
		if _, err := w.WriteString(":" + strconv.FormatInt(val, 10) + "\r\n"); err != nil {
			return err
		}
		return nil
	case float32:
		return encodeValue(w, float64(val))
	case float64:
		if _, err := w.WriteString("," + strconv.FormatFloat(val, 'f', -1, 64) + "\r\n"); err != nil {
			return err
		}
		return nil
	case bool:
		if val {
			_, err := w.WriteString("#t\r\n")
			return err
		}
		_, err := w.WriteString("#f\r\n")
		return err
	case time.Time:
		if _, err := w.WriteString("@" + val.UTC().Format(time.RFC3339) + "\r\n"); err != nil {
			return err
		}
		return nil
	case []any:
		if _, err := w.WriteString("*" + strconv.Itoa(len(val)) + "\r\n"); err != nil {
			return err
		}
		for _, it := range val {
			if err := encodeValue(w, it); err != nil {
				return err
			}
		}
		return nil
	case []string:
		arr := make([]any, len(val))
		for i, s := range val {
			arr[i] = s
		}
		return encodeValue(w, arr)
	case []int:
		arr := make([]any, len(val))
		for i, s := range val {
			arr[i] = s
		}
		return encodeValue(w, arr)
	case map[string]any:
		if _, err := w.WriteString("%" + strconv.Itoa(len(val)) + "\r\n"); err != nil {
			return err
		}
		keys := make([]string, 0, len(val))
		for k := range val {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			if err := encodeValue(w, k); err != nil {
				return err
			}
			if err := encodeValue(w, val[k]); err != nil {
				return err
			}
		}
		return nil
	case map[string]string:
		m := make(map[string]any, len(val))
		for k, v := range val {
			m[k] = v
		}
		return encodeValue(w, m)
	default:
		return fmt.Errorf("unsupported type %T", v)
	}
}

// Decoder reads BMON values from an io.Reader.
type Decoder struct {
	r *bufio.Reader
}

// NewDecoder returns a new Decoder.
func NewDecoder(r io.Reader) *Decoder {
	return &Decoder{r: bufio.NewReader(r)}
}

// Decode reads the next BMON value from the stream into v.
// v must be a non-nil pointer.
func (d *Decoder) Decode(v any) error {
	val, err := d.decodeValue()
	if err != nil {
		return err
	}
	rv := reflect.ValueOf(v)
	if rv.Kind() != reflect.Pointer || rv.IsNil() {
		return fmt.Errorf("Decode argument must be non-nil pointer")
	}
	rv = rv.Elem()
	if val == nil {
		rv.Set(reflect.Zero(rv.Type()))
	} else {
		rv.Set(reflect.ValueOf(val))
	}
	return nil
}

func (d *Decoder) decodeValue() (any, error) {
	prefix, err := d.r.ReadByte()
	if err != nil {
		return nil, err
	}
	switch prefix {
	case '+':
		line, err := d.readLine()
		if err != nil {
			return nil, err
		}
		return line, nil
	case '-':
		line, err := d.readLine()
		if err != nil {
			return nil, err
		}
		return errors.New(line), nil
	case ':':
		line, err := d.readLine()
		if err != nil {
			return nil, err
		}
		n, err := strconv.ParseInt(line, 10, 64)
		if err != nil {
			return nil, err
		}
		return n, nil
	case ',':
		line, err := d.readLine()
		if err != nil {
			return nil, err
		}
		f, err := strconv.ParseFloat(line, 64)
		if err != nil {
			return nil, err
		}
		return f, nil
	case '#':
		line, err := d.readLine()
		if err != nil {
			return nil, err
		}
		return line == "t", nil
	case '_':
		if err := d.expectCRLF(); err != nil {
			return nil, err
		}
		return nil, nil
	case '$':
		line, err := d.readLine()
		if err != nil {
			return nil, err
		}
		n, err := strconv.Atoi(line)
		if err != nil {
			return nil, err
		}
		buf := make([]byte, n+2)
		if _, err := io.ReadFull(d.r, buf); err != nil {
			return nil, err
		}
		if buf[n] != '\r' || buf[n+1] != '\n' {
			return nil, fmt.Errorf("invalid blob termination")
		}
		return buf[:n], nil
	case '=':
		line, err := d.readLine()
		if err != nil {
			return nil, err
		}
		n, err := strconv.Atoi(line)
		if err != nil {
			return nil, err
		}
		buf := make([]byte, n+2)
		if _, err := io.ReadFull(d.r, buf); err != nil {
			return nil, err
		}
		if buf[n] != '\r' || buf[n+1] != '\n' {
			return nil, fmt.Errorf("invalid verbatim termination")
		}
		return string(buf[:n]), nil
	case '*':
		line, err := d.readLine()
		if err != nil {
			return nil, err
		}
		n, err := strconv.Atoi(line)
		if err != nil {
			return nil, err
		}
		arr := make([]any, 0, n)
		for i := 0; i < n; i++ {
			v, err := d.decodeValue()
			if err != nil {
				return nil, err
			}
			arr = append(arr, v)
		}
		return arr, nil
	case '%':
		line, err := d.readLine()
		if err != nil {
			return nil, err
		}
		n, err := strconv.Atoi(line)
		if err != nil {
			return nil, err
		}
		m := make(map[string]any, n)
		for i := 0; i < n; i++ {
			key, err := d.decodeValue()
			if err != nil {
				return nil, err
			}
			ks, ok := key.(string)
			if !ok {
				return nil, fmt.Errorf("map key not string: %T", key)
			}
			val, err := d.decodeValue()
			if err != nil {
				return nil, err
			}
			m[ks] = val
		}
		return m, nil
	case '~':
		line, err := d.readLine()
		if err != nil {
			return nil, err
		}
		n, err := strconv.Atoi(line)
		if err != nil {
			return nil, err
		}
		arr := make([]any, 0, n)
		for i := 0; i < n; i++ {
			v, err := d.decodeValue()
			if err != nil {
				return nil, err
			}
			arr = append(arr, v)
		}
		return arr, nil
	case '@':
		line, err := d.readLine()
		if err != nil {
			return nil, err
		}
		t, err := time.Parse(time.RFC3339, line)
		if err != nil {
			return nil, err
		}
		return t, nil
	default:
		return nil, fmt.Errorf("unknown prefix %q", prefix)
	}
}

func (d *Decoder) readLine() (string, error) {
	line, err := d.r.ReadString('\n')
	if err != nil {
		return "", err
	}
	if len(line) < 2 || line[len(line)-2] != '\r' {
		return "", fmt.Errorf("invalid line ending")
	}
	return line[:len(line)-2], nil
}

func (d *Decoder) expectCRLF() error {
	b := make([]byte, 2)
	if _, err := io.ReadFull(d.r, b); err != nil {
		return err
	}
	if b[0] != '\r' || b[1] != '\n' {
		return fmt.Errorf("expected CRLF")
	}
	return nil
}
