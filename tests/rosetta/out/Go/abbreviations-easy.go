//go:build ignore

package main

import (
	"fmt"
	"strings"
)

// line 1
func fields(s string) []string {
	var words []string = []string{}
	var cur string = ""
	var i int = 0
	for {
		if !(i < len(s)) {
			break
		}
		var ch string = _sliceString(s, i, (i + 1))
		if ((ch == " ") || (ch == "\n")) || (ch == "\t") {
			if len(cur) > 0 {
				words = append(_convSlice[string, any](words), cur)
				cur = ""
			}
		} else {
			cur = cur + ch
		}
		i = (i + 1)
	}
	if len(cur) > 0 {
		words = append(_convSlice[string, any](words), cur)
	}
	return words
}

// line 23
func padRight(s string, width int) string {
	var out string = s
	var i int = len(s)
	for {
		if !(i < width) {
			break
		}
		out = out + " "
		i = (i + 1)
	}
	return out
}

// line 33
func join(xs []string, sep string) string {
	var res string = ""
	var i int = 0
	for {
		if !(i < len(xs)) {
			break
		}
		if i > 0 {
			res = res + sep
		}
		res = res + xs[i]
		i = (i + 1)
	}
	return res
}

// line 46
func validate(commands []string, words []string, mins []int) []string {
	var results []string = []string{}
	if len(words) == 0 {
		return results
	}
	var wi int = 0
	for {
		if !(wi < len(words)) {
			break
		}
		var w string = words[wi]
		var found bool = false
		var wlen int = len(w)
		var ci int = 0
		for {
			if !(ci < len(commands)) {
				break
			}
			var cmd string = commands[ci]
			if ((mins[ci] != 0) && (wlen >= mins[ci])) && (wlen <= len(cmd)) {
				var c string = strings.ToUpper(cmd)
				var ww string = strings.ToUpper(w)
				if _sliceString(c, 0, wlen) == ww {
					results = append(_convSlice[string, any](results), c)
					found = true
					break
				}
			}
			ci = (ci + 1)
		}
		if !found {
			results = append(_convSlice[string, any](results), "*error*")
		}
		wi = (wi + 1)
	}
	return results
}

// line 78
func main() {
	var table string = "Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress Copy " + "COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find " + "NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput " + " Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO " + "MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT " + "READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT " + "RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer TypeUp "
	var commands []string = fields(table)
	var mins []int = []int{}
	var i int = 0
	for {
		if !(i < len(commands)) {
			break
		}
		var count int = 0
		var j int = 0
		var cmd string = commands[i]
		for {
			if !(j < len(cmd)) {
				break
			}
			var ch string = _sliceString(cmd, j, (j + 1))
			if (ch >= "A") && (ch <= "Z") {
				count = (count + 1)
			}
			j = (j + 1)
		}
		mins = append(_convSlice[int, any](mins), count)
		i = (i + 1)
	}
	var sentence string = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"
	var words []string = fields(sentence)
	var results []string = validate(commands, words, mins)
	var out1 string = "user words:  "
	var k int = 0
	for {
		if !(k < len(words)) {
			break
		}
		out1 = out1 + padRight(words[k], len(results[k])) + " "
		k = (k + 1)
	}
	fmt.Println(out1)
	fmt.Println("full words:  " + join(results, " "))
}

func main() {
	main()
}

func _convSlice[T any, U any](s []T) []U {
	out := []U{}
	for _, v := range s {
		out = append(out, any(v).(U))
	}
	return out
}

func _sliceString(s string, i, j int) string {
	start := i
	end := j
	n := len([]rune(s))
	if start < 0 {
		start += n
	}
	if end < 0 {
		end += n
	}
	if start < 0 {
		start = 0
	}
	if end > n {
		end = n
	}
	if end < start {
		end = start
	}
	return string([]rune(s)[start:end])
}
