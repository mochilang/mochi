package goffi_test

import (
    "testing"

    goffi "mochi/runtime/ffi/go"
)

// TestRealInfer verifies inference on a standard library package.
func TestRealInfer(t *testing.T) {
    info, err := goffi.Infer("database/sql")
    if err != nil {
        t.Fatalf("infer failed: %v", err)
    }

    if info.Path != "database/sql" {
        t.Fatalf("unexpected module path %s", info.Path)
    }
    if info.Doc == "" {
        t.Fatalf("expected package documentation")
    }

    foundRegister := false
    for _, f := range info.Functions {
        if f.Name == "Register" {
            foundRegister = true
            if len(f.Params) != 2 || f.Params[0].Type != "string" || f.Params[1].Type == "" {
                t.Fatalf("Register params incorrect: %+v", f.Params)
            }
            break
        }
    }
    if !foundRegister {
        t.Fatalf("Register function not found")
    }

    foundErrNoRows := false
    for _, v := range info.Vars {
        if v.Name == "ErrNoRows" {
            foundErrNoRows = true
            if v.Type != "error" {
                t.Fatalf("ErrNoRows type incorrect: %s", v.Type)
            }
            break
        }
    }
    if !foundErrNoRows {
        t.Fatalf("ErrNoRows variable not found")
    }

    foundDB := false
    for _, tinfo := range info.Types {
        if tinfo.Name == "DB" {
            foundDB = true
            if tinfo.Kind != "struct" {
                t.Fatalf("DB kind should be struct, got %s", tinfo.Kind)
            }
            if len(tinfo.Methods) == 0 {
                t.Fatalf("expected DB methods")
            }
            break
        }
    }
    if !foundDB {
        t.Fatalf("DB type not found")
    }
}

