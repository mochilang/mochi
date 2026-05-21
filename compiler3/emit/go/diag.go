package gogen

import (
	"bufio"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
)

// FilterBuildErrors rewrites `go build` stderr so diagnostics that
// reference the emitted Go file are remapped to Mochi-source
// coordinates using the `//line` directives the emitter wrote.
//
// The Go toolchain already honours `//line` directives, so most errors
// arrive with Mochi coords baked in. This filter handles the residual
// cases:
//   - errors emitted by `go build` itself (load/typecheck phase) that
//     reference the gen file directly without honouring `//line`;
//   - errors that point at a line of the gen file with no preceding
//     `//line` directive (e.g. the package clause).
//
// genPath is the absolute or relative path of the emitted Go file as
// it appears in the toolchain's stderr. genSrc is the source the
// emitter produced; FilterBuildErrors scans it once to build a line-
// to-(file:line) map. stderr is the raw `go build` output.
func FilterBuildErrors(genPath, genSrc, stderr string) string {
	dirMap := buildLineMap(genSrc)
	genBase := filepath.Base(genPath)

	// A diagnostic line looks like:
	//   path/to/file.go:LINE:COL: message
	// or
	//   path/to/file.go:LINE: message
	// We rewrite only when the path matches genPath (or its basename),
	// or when the file appears to be a Mochi source we want to leave
	// untouched.
	re := regexp.MustCompile(`^(.*?):(\d+)(?::(\d+))?:\s*(.*)$`)

	var out strings.Builder
	scanner := bufio.NewScanner(strings.NewReader(stderr))
	for scanner.Scan() {
		line := scanner.Text()
		m := re.FindStringSubmatch(strings.TrimSpace(line))
		if m == nil {
			out.WriteString(line)
			out.WriteByte('\n')
			continue
		}
		path := m[1]
		// Match either the full path or just the basename: `go build`
		// reports paths relative to the working directory, which may
		// differ from how the caller named the file.
		if path != genPath && filepath.Base(path) != genBase {
			out.WriteString(line)
			out.WriteByte('\n')
			continue
		}
		n, err := strconv.Atoi(m[2])
		if err != nil {
			out.WriteString(line)
			out.WriteByte('\n')
			continue
		}
		mapped := remapLine(dirMap, n)
		if mapped == "" {
			out.WriteString(line)
			out.WriteByte('\n')
			continue
		}
		col := m[3]
		msg := m[4]
		if col != "" {
			out.WriteString(mapped)
			out.WriteString(": ")
			out.WriteString(msg)
		} else {
			out.WriteString(mapped)
			out.WriteString(": ")
			out.WriteString(msg)
		}
		out.WriteByte('\n')
	}
	return out.String()
}

// lineDirective is one `//line file:N` parsed entry. From holds the
// 1-based line number in the generated file at which the directive
// appears; the next physical line of generated source is rewritten
// to (File, BaseLine).
type lineDirective struct {
	From     int
	File     string
	BaseLine int
}

// buildLineMap scans genSrc and returns the ordered list of //line
// directives.
func buildLineMap(genSrc string) []lineDirective {
	var dirs []lineDirective
	scanner := bufio.NewScanner(strings.NewReader(genSrc))
	scanner.Buffer(make([]byte, 0, 64*1024), 1024*1024)
	lineNum := 0
	for scanner.Scan() {
		lineNum++
		txt := scanner.Text()
		if !strings.HasPrefix(txt, "//line ") {
			continue
		}
		body := strings.TrimSpace(txt[len("//line "):])
		idx := strings.LastIndex(body, ":")
		if idx < 0 {
			continue
		}
		file := body[:idx]
		n, err := strconv.Atoi(body[idx+1:])
		if err != nil {
			continue
		}
		dirs = append(dirs, lineDirective{From: lineNum, File: file, BaseLine: n})
	}
	return dirs
}

// remapLine returns "file:N" for a 1-based generated-file line, using
// the directive table. Returns "" when no directive precedes the line.
func remapLine(dirs []lineDirective, genLine int) string {
	var active *lineDirective
	for i := range dirs {
		if dirs[i].From >= genLine {
			break
		}
		active = &dirs[i]
	}
	if active == nil {
		return ""
	}
	// The line immediately after the directive (genLine = active.From+1)
	// maps to active.BaseLine; each further generated line increments.
	delta := genLine - active.From - 1
	return active.File + ":" + strconv.Itoa(active.BaseLine+delta)
}
