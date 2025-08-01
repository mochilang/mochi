//go:build ignore

// Generated by Mochi v0.10.55 on 2025-08-02 18:06:34 GMT+7
package main

import (
	"encoding/json"
	"fmt"
	"os"
	"runtime"
	"strconv"
	"strings"
	"time"
)

var seededNow bool
var nowSeed int64

func init() {
	if s := os.Getenv("MOCHI_NOW_SEED"); s != "" {
		if v, err := strconv.ParseInt(s, 10, 64); err == nil {
			nowSeed = v
			seededNow = true
		}
	}
}
func _now() int {
	if seededNow {
		nowSeed = (nowSeed*1664525 + 1013904223) % 2147483647
		return int(nowSeed)
	}
	return int(time.Now().UnixNano())
}

func _repeat(s string, n int) string {
	if n <= 0 {
		return ""
	}
	return strings.Repeat(s, n)
}

var rows int

var cols int

var p float64

var f float64

func repeat(ch string, n int) string {
	var s string = ""
	_ = s
	var i int = 0
	_ = i
	for i < n {
		s = (s + ch)
		i = (i + 1)
	}
	return s
}

func chance(prob float64) bool {
	var threshold int = int((prob * 1000.0))
	_ = threshold
	_ = threshold
	return ((_now() % 1000) < threshold)
}

func newBoard() [][]string {
	var b [][]string = [][]string{}
	_ = b
	var r int = 0
	_ = r
	for r < rows {
		var row []string = []string{}
		_ = row
		var c int = 0
		_ = c
		for c < cols {
			if (_now() % 2) == 0 {
				row = append(row, "T")
			} else {
				row = append(row, " ")
			}
			c = (c + 1)
		}
		b = append(b, row)
		r = (r + 1)
	}
	return b
}

func step(src [][]string) [][]string {
	var dst [][]string = [][]string{}
	_ = dst
	var r int = 0
	_ = r
	for r < rows {
		var row []string = []string{}
		_ = row
		var c int = 0
		_ = c
		for c < cols {
			var cell string = src[r][c]
			_ = cell
			var next string = cell
			_ = next
			if cell == "#" {
				next = " "
			} else {
				if cell == "T" {
					var burning bool = false
					_ = burning
					var dr int = (0 - 1)
					_ = dr
					for dr <= 1 {
						var dc int = (0 - 1)
						_ = dc
						for dc <= 1 {
							if (dr != 0) || (dc != 0) {
								var rr int = (r + dr)
								_ = rr
								var cc int = (c + dc)
								_ = cc
								if (((rr >= 0) && (rr < rows)) && (cc >= 0)) && (cc < cols) {
									if src[rr][cc] == "#" {
										burning = true
									}
								}
							}
							dc = (dc + 1)
						}
						dr = (dr + 1)
					}
					if burning || chance(f) {
						next = "#"
					}
				} else {
					if chance(p) {
						next = "T"
					}
				}
			}
			row = append(row, next)
			c = (c + 1)
		}
		dst = append(dst, row)
		r = (r + 1)
	}
	return dst
}

func printBoard(b [][]string) {
	fmt.Println((_repeat("__", cols) + "\n\n"))
	var r int = 0
	_ = r
	for r < rows {
		var line string = ""
		_ = line
		var c int = 0
		_ = c
		for c < cols {
			var cell string = b[r][c]
			_ = cell
			if cell == " " {
				line = (line + "  ")
			} else {
				line = ((line + " ") + cell)
			}
			c = (c + 1)
		}
		fmt.Println((line + "\n"))
		r = (r + 1)
	}
}

var board [][]string

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		rows = 20
		cols = 30
		p = 0.01
		f = 0.001
		board = newBoard()
		printBoard(func(v any) [][]string {
			if v == nil {
				return nil
			}
			if vv, ok := v.([][]string); ok {
				return vv
			}
			if arr, ok := v.([]any); ok {
				if len(arr) == 0 {
					return [][]string{}
				}
				out := make([][]string, len(arr))
				for i, x := range arr {
					out[i] = func(v any) []string {
						if v == nil {
							return nil
						}
						if vv, ok := v.([]string); ok {
							return vv
						}
						if arr, ok := v.([]any); ok {
							if len(arr) == 0 {
								return []string{}
							}
							out := make([]string, len(arr))
							for i, x := range arr {
								out[i] = x.(string)
							}
							return out
						}
						return v.([]string)
					}(x)
				}
				return out
			}
			return v.([][]string)
		}(board))
		board = step(func(v any) [][]string {
			if v == nil {
				return nil
			}
			if vv, ok := v.([][]string); ok {
				return vv
			}
			if arr, ok := v.([]any); ok {
				if len(arr) == 0 {
					return [][]string{}
				}
				out := make([][]string, len(arr))
				for i, x := range arr {
					out[i] = func(v any) []string {
						if v == nil {
							return nil
						}
						if vv, ok := v.([]string); ok {
							return vv
						}
						if arr, ok := v.([]any); ok {
							if len(arr) == 0 {
								return []string{}
							}
							out := make([]string, len(arr))
							for i, x := range arr {
								out[i] = x.(string)
							}
							return out
						}
						return v.([]string)
					}(x)
				}
				return out
			}
			return v.([][]string)
		}(board))
		printBoard(func(v any) [][]string {
			if v == nil {
				return nil
			}
			if vv, ok := v.([][]string); ok {
				return vv
			}
			if arr, ok := v.([]any); ok {
				if len(arr) == 0 {
					return [][]string{}
				}
				out := make([][]string, len(arr))
				for i, x := range arr {
					out[i] = func(v any) []string {
						if v == nil {
							return nil
						}
						if vv, ok := v.([]string); ok {
							return vv
						}
						if arr, ok := v.([]any); ok {
							if len(arr) == 0 {
								return []string{}
							}
							out := make([]string, len(arr))
							for i, x := range arr {
								out[i] = x.(string)
							}
							return out
						}
						return v.([]string)
					}(x)
				}
				return out
			}
			return v.([][]string)
		}(board))
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
