package main

import (
    "bufio"
    "bytes"
    "encoding/csv"
    "fmt"
    "io"
    "net/http"
    "os"
    "path/filepath"
    "regexp"
    "strconv"
    "strings"

    "mochi/parser"
    mod "mochi/runtime/mod"
    "mochi/runtime/vm"
    "mochi/types"
)

type Table struct {
    Name    string
    Columns []string
    Rows    [][]string
}

type Query struct {
    Func        string
    Distinct    bool
    Column      string
    Table       string
    WhereColumn string
    WhereValue  string
}

type TestCase struct {
    Table    Table
    Query    Query
    Expected string
}

func findRepoRoot() (string, error) {
    dir, err := os.Getwd()
    if err != nil {
        return "", err
    }
    for i := 0; i < 10; i++ {
        if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
            return dir, nil
        }
        parent := filepath.Dir(dir)
        if parent == dir {
            break
        }
        dir = parent
    }
    return "", fmt.Errorf("go.mod not found")
}

const repoBase = "https://raw.githubusercontent.com/cockroachdb/sqllogictest/master/test/evidence/"
var testFiles = []string{"slt_lang_aggfunc.test"}

func downloadTests(dir string) {
    os.MkdirAll(dir, 0755)
    for _, name := range testFiles {
        path := filepath.Join(dir, name)
        if _, err := os.Stat(path); err == nil {
            continue
        }
        url := repoBase + name
        resp, err := http.Get(url)
        if err != nil {
            fmt.Fprintf(os.Stderr, "download %s: %v\n", url, err)
            continue
        }
        data, err := io.ReadAll(resp.Body)
        resp.Body.Close()
        if err != nil {
            fmt.Fprintf(os.Stderr, "read %s: %v\n", url, err)
            continue
        }
        os.WriteFile(path, data, 0644)
    }
}

var createRe = regexp.MustCompile(`(?i)^CREATE TABLE (\w+)\((.+)\)`)
var insertRe = regexp.MustCompile(`(?i)^INSERT INTO (\w+)(?:\(([^)]*)\))? VALUES\(([^)]*)\)`)
var queryRe = regexp.MustCompile(`(?i)^SELECT\s+([a-z_]+)\((?:DISTINCT\s+)?(\*|\w+)\)\s+FROM\s+(\w+)(?:\s+WHERE\s+(\w+)='?([^']*)'?)*`)

func parseValues(s string) []string {
    r := csv.NewReader(strings.NewReader(s))
    r.TrimLeadingSpace = true
    fields, err := r.Read()
    if err != nil {
        parts := strings.Split(s, ",")
        for i := range parts {
            parts[i] = strings.TrimSpace(parts[i])
        }
        return parts
    }
    for i := range fields {
        fields[i] = strings.TrimSpace(fields[i])
    }
    return fields
}

func parseFile(path string) ([]TestCase, error) {
    f, err := os.Open(path)
    if err != nil {
        return nil, err
    }
    defer f.Close()

    scanner := bufio.NewScanner(f)
    var table Table
    var cases []TestCase
    for scanner.Scan() {
        line := strings.TrimSpace(scanner.Text())
        if strings.HasPrefix(line, "CREATE TABLE") {
            if m := createRe.FindStringSubmatch(line); m != nil {
                table.Name = m[1]
                cols := strings.Split(m[2], ",")
                for _, c := range cols {
                    parts := strings.Fields(strings.TrimSpace(c))
                    if len(parts) > 0 {
                        table.Columns = append(table.Columns, parts[0])
                    }
                }
            }
        } else if strings.HasPrefix(line, "INSERT INTO") {
            if m := insertRe.FindStringSubmatch(line); m != nil {
                colOrder := table.Columns
                if m[2] != "" {
                    cols := strings.Split(m[2], ",")
                    colOrder = make([]string, 0, len(cols))
                    for _, c := range cols {
                        colOrder = append(colOrder, strings.TrimSpace(c))
                    }
                }
                vals := parseValues(m[3])
                row := make([]string, len(table.Columns))
                for i, col := range colOrder {
                    idx := -1
                    for j, name := range table.Columns {
                        if name == col {
                            idx = j
                            break
                        }
                    }
                    if idx >= 0 && i < len(vals) {
                        row[idx] = strings.TrimSpace(vals[i])
                    }
                }
                table.Rows = append(table.Rows, row)
            }
        } else if strings.HasPrefix(line, "query") {
            if !scanner.Scan() {
                break
            }
            qline := strings.TrimSpace(scanner.Text())
            m := queryRe.FindStringSubmatch(qline)
            if m == nil {
                continue
            }
            fn := strings.ToLower(m[1])
            if fn == "total" {
                fn = "sum"
            }
            q := Query{
                Func:   fn,
                Column: m[2],
                Table:  m[3],
            }
            if strings.Contains(strings.ToLower(qline), "distinct") {
                q.Distinct = true
            }
            if m[4] != "" {
                q.WhereColumn = m[4]
                q.WhereValue = m[5]
            }
            if !scanner.Scan() {
                break
            }
            if strings.TrimSpace(scanner.Text()) != "----" {
                continue
            }
            if !scanner.Scan() {
                break
            }
            expected := strings.TrimSpace(scanner.Text())
            if q.Func == "avg" {
                continue
            }
            cases = append(cases, TestCase{Table: table, Query: q, Expected: expected})
            if len(cases) >= 4 {
                break
            }
        }
    }
    return cases, scanner.Err()
}

func valueLit(s string) string {
    s = strings.TrimSpace(s)
    if s == "" {
        return "null"
    }
    if strings.EqualFold(s, "null") {
        return "null"
    }
    if _, err := strconv.Atoi(s); err == nil {
        return s
    }
    if _, err := strconv.ParseFloat(s, 64); err == nil {
        return s
    }
    if strings.HasPrefix(s, "'") && strings.HasSuffix(s, "'") {
        return "\"" + strings.Trim(s, "'") + "\""
    }
    if !strings.HasPrefix(s, "\"") {
        return "\"" + s + "\""
    }
    return s
}

func generateMochi(tc TestCase) []byte {
    var b bytes.Buffer
    if tc.Query.Distinct {
        b.WriteString("fun distinct(xs: list<any>): list<any> {\n")
        b.WriteString("  let out = []\n")
        b.WriteString("  for x in xs {\n")
        b.WriteString("    if !out.contains(x) {\n")
        b.WriteString("      out = append(out, x)\n")
        b.WriteString("    }\n")
        b.WriteString("  }\n")
        b.WriteString("  out\n")
        b.WriteString("}\n\n")
    }
    fmt.Fprintf(&b, "let %s = [\n", tc.Table.Name)
    for _, row := range tc.Table.Rows {
        b.WriteString("  {")
        for i, col := range tc.Table.Columns {
            if i > 0 {
                b.WriteString(", ")
            }
            val := row[i]
            if val == "" {
                b.WriteString(fmt.Sprintf("%s: null", col))
            } else {
                b.WriteString(fmt.Sprintf("%s: %s", col, valueLit(val)))
            }
        }
        b.WriteString("},\n")
    }
    b.WriteString("]\n\n")
    filterParts := []string{}
    if tc.Query.WhereColumn != "" {
        filterParts = append(filterParts, fmt.Sprintf("r.%s == %s", tc.Query.WhereColumn, valueLit(tc.Query.WhereValue)))
    }
    if tc.Query.Column != "*" {
        filterParts = append(filterParts, fmt.Sprintf("r.%s != null", tc.Query.Column))
    }
    filter := ""
    if len(filterParts) > 0 {
        filter = " where " + strings.Join(filterParts, " && ")
    }
    inner := ""
    if tc.Query.Column == "*" {
        inner = fmt.Sprintf("from r in %s%s select 1", tc.Query.Table, filter)
    } else {
        inner = fmt.Sprintf("from r in %s%s select r.%s", tc.Query.Table, filter, tc.Query.Column)
    }
    if tc.Query.Distinct {
        inner = fmt.Sprintf("distinct(%s)", inner)
    }
    expr := fmt.Sprintf("%s(%s)", tc.Query.Func, inner)
    fmt.Fprintf(&b, "let result = %s\n", expr)
    fmt.Fprintln(&b, "print(result)")
    fmt.Fprintf(&b, "\ntest \"%s\" {\n  expect result == %s\n}\n", tc.Query.Func, valueLit(tc.Expected))
    return b.Bytes()
}

func runMochi(src, outDir string) error {
    base := strings.TrimSuffix(filepath.Base(src), ".mochi")
    outPath := filepath.Join(outDir, base+".out")
    irPath := filepath.Join(outDir, base+".ir.out")

    prog, err := parser.Parse(src)
    if err != nil {
        return fmt.Errorf("parse: %w", err)
    }
    env := types.NewEnv(nil)
    if errs := types.Check(prog, env); len(errs) > 0 {
        return fmt.Errorf("type: %v", errs[0])
    }
    modRoot, errRoot := mod.FindRoot(filepath.Dir(src))
    if errRoot != nil {
        modRoot = filepath.Dir(src)
    }
    os.Setenv("MOCHI_ROOT", modRoot)
    p, err := vm.Compile(prog, env)
    if err != nil {
        return fmt.Errorf("compile: %w", err)
    }
    var buf bytes.Buffer
    m := vm.New(p, &buf)
    err = m.Run()
    outBytes := []byte(strings.TrimSpace(buf.String()) + "\n")
    if werr := os.WriteFile(outPath, outBytes, 0644); werr != nil {
        return fmt.Errorf("write out: %w", werr)
    }
    srcData, err2 := os.ReadFile(src)
    if err2 == nil {
        ir := []byte(strings.TrimSpace(p.Disassemble(string(srcData))))
        os.WriteFile(irPath, ir, 0644)
    }
    return err
}

func main() {
    root, err := findRepoRoot()
    if err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(1)
    }
    dir := filepath.Join(root, "tests/dataset/logic")
    downloadTests(dir)
    outDir := filepath.Join(dir, "out")
    os.MkdirAll(outDir, 0755)
    for _, name := range testFiles {
        path := filepath.Join(dir, name)
        cases, err := parseFile(path)
        if err != nil {
            fmt.Fprintf(os.Stderr, "%s: %v\n", name, err)
            continue
        }
        base := strings.TrimSuffix(name, ".test")
        for i, c := range cases {
            src := filepath.Join(dir, fmt.Sprintf("%s_%d.mochi", base, i+1))
            os.WriteFile(src, generateMochi(c), 0644)
            if err := runMochi(src, outDir); err != nil {
                fmt.Fprintf(os.Stderr, "%s: %v\n", src, err)
            }
        }
    }
}

