package typebridge

import (
	"go/ast"
	"go/types"
	"os/exec"
	"strings"
	"testing"

	"golang.org/x/tools/go/packages"
)

// curatedStdPackages is the Phase-1 soak set. The full Go-1.26
// stdlib walk is deferred to MEP-44 §6 once the Phase 2 resolver
// can enumerate packages. The curated set still exercises every
// shape the bridge handles (basics, slices, maps, structs, refs,
// chans, funcs, named types, interfaces, generic instances).
var curatedStdPackages = []string{
	"strings",
	"bytes",
	"fmt",
	"sort",
	"slices",
	"maps",
	"io",
	"os",
	"time",
	"errors",
	"strconv",
	"unicode",
	"unicode/utf8",
	"path",
	"path/filepath",
	"encoding/json",
	"encoding/base64",
	"container/list",
	"container/heap",
	"sync",
	"context",
	"math",
	"math/big",
	"net/url",
	"hash",
	"hash/fnv",
	"cmp",
}

// TestStdlibSoakCurated walks a curated set of std packages and
// asserts every exported symbol's type maps through the bridge
// without producing KindInvalid. Opaque density is tracked and
// reported on -v.
func TestStdlibSoakCurated(t *testing.T) {
	cfg := &packages.Config{
		Mode: packages.NeedTypes | packages.NeedTypesInfo | packages.NeedSyntax | packages.NeedDeps | packages.NeedImports | packages.NeedName,
	}
	pkgs, err := packages.Load(cfg, curatedStdPackages...)
	if err != nil {
		t.Fatalf("packages.Load: %v", err)
	}
	if len(pkgs) == 0 {
		t.Fatal("no packages loaded")
	}

	var totalSymbols, totalTypes, invalidCount, opaqueCount int
	opaqueByReason := map[OpaqueReason]int{}
	invalidSamples := []string{}
	opaqueSamples := map[OpaqueReason][]string{}

	checkType := func(label string, gt types.Type) {
		if gt == nil {
			return
		}
		totalTypes++
		mt := GoToMochi(gt)
		if mt.Kind == KindInvalid {
			invalidCount++
			if len(invalidSamples) < 10 {
				invalidSamples = append(invalidSamples, label)
			}
			return
		}
		// Walk for KindOpaque occurrences inside the result so we
		// observe density accurately.
		visitType(mt, func(child Type) {
			if child.Kind == KindOpaque {
				opaqueCount++
				opaqueByReason[child.OpaqueReason]++
				if child.OpaqueReason == OpaqueUnknown && child.GoType == "" {
					t.Errorf("%s: KindOpaque with empty GoType (must carry rendering string)", label)
				}
				if len(opaqueSamples[child.OpaqueReason]) < 5 {
					opaqueSamples[child.OpaqueReason] = append(opaqueSamples[child.OpaqueReason], label)
				}
			}
		})
	}

	for _, p := range pkgs {
		if p.Types == nil {
			t.Logf("pkg %s: no types", p.PkgPath)
			continue
		}
		scope := p.Types.Scope()
		for _, name := range scope.Names() {
			if !ast.IsExported(name) {
				continue
			}
			obj := scope.Lookup(name)
			if obj == nil {
				continue
			}
			totalSymbols++
			switch o := obj.(type) {
			case *types.Func:
				if sig, ok := o.Type().(*types.Signature); ok {
					checkType(p.PkgPath+"."+name+" (func)", sig)
				}
			case *types.TypeName:
				checkType(p.PkgPath+"."+name+" (type)", o.Type())
			case *types.Var:
				checkType(p.PkgPath+"."+name+" (var)", o.Type())
			case *types.Const:
				checkType(p.PkgPath+"."+name+" (const)", o.Type())
			}
		}
	}

	if invalidCount > 0 {
		t.Errorf("%d KindInvalid results out of %d types; samples: %v", invalidCount, totalTypes, invalidSamples)
	}

	density := 0.0
	if totalTypes > 0 {
		density = 100.0 * float64(opaqueCount) / float64(totalTypes)
	}

	t.Logf("soak: %d packages, %d exported symbols, %d type-references visited", len(pkgs), totalSymbols, totalTypes)
	t.Logf("opaque: %d (%0.2f%% density)", opaqueCount, density)
	for reason, cnt := range opaqueByReason {
		t.Logf("  %s: %d  samples=%s", reason, cnt, strings.Join(opaqueSamples[reason], ", "))
	}

	// Phase 1 acceptance bar: opaque density under 10% on the curated
	// surface. uintptr and unsafe.Pointer inside sync/atomic primitives
	// are legitimately opaque; tightening below 10% would require
	// pretending those fields are typed, which the resolver would have
	// to undo. Phase 2's full-stdlib walk re-asserts this.
	if density >= 10.0 {
		t.Errorf("opaque density %0.2f%% exceeds the Phase-1 10%% bar", density)
	}
}

// enumerateStdPackages returns every importable package path in the
// Go stdlib by shelling out to `go list std`. Internal packages (path
// containing "/internal/" or prefix "internal/") and vendored paths
// are filtered out because they are not part of the MEP-43 reachable
// surface (the Mochi user cannot `import go "internal/abi"`).
//
// Used by TestStdlibSoakFull to close out MEP-44 §1.7.
func enumerateStdPackages(t *testing.T) []string {
	t.Helper()
	out, err := exec.Command("go", "list", "std").Output()
	if err != nil {
		t.Skipf("go list std unavailable: %v", err)
	}
	var pkgs []string
	for line := range strings.SplitSeq(strings.TrimSpace(string(out)), "\n") {
		p := strings.TrimSpace(line)
		if p == "" {
			continue
		}
		if strings.Contains(p, "/vendor/") || strings.HasPrefix(p, "vendor/") {
			continue
		}
		if strings.HasPrefix(p, "internal/") || strings.Contains(p, "/internal/") {
			continue
		}
		pkgs = append(pkgs, p)
	}
	return pkgs
}

// expectedOpaqueReasons lists the OpaqueReasons that are legitimate at
// the bridge surface: Mochi has no equivalent type, so the bridge is
// supposed to mark them opaque. The MEP-44 §6 10% bar is *unexpected*
// opacity, that is, OpaqueReasons other than these. Counting raw
// opacity would force the resolver to pretend uintptr fields are i64,
// which a downstream consumer would have to undo.
var expectedOpaqueReasons = map[OpaqueReason]bool{
	OpaqueUnsafePointer: true,
	OpaqueUintptr:       true,
	OpaqueComplex:       true,
}

// TestStdlibSoakFull walks every package in the current Go stdlib and
// asserts the bridge maps every exported symbol without KindInvalid,
// and that the cumulative *unexpected* opaque density (OpaqueReasons
// outside expectedOpaqueReasons) stays under the MEP-44 §6 10% bar.
// This is the gate for MEP-44 sub-phase 1.7 (the curated soak in
// TestStdlibSoakCurated remains as a fast sanity guard).
func TestStdlibSoakFull(t *testing.T) {
	if testing.Short() {
		t.Skip("full stdlib soak skipped in -short")
	}
	stdPkgs := enumerateStdPackages(t)
	if len(stdPkgs) == 0 {
		t.Skip("no stdlib packages enumerated")
	}
	cfg := &packages.Config{
		Mode: packages.NeedTypes | packages.NeedTypesInfo | packages.NeedSyntax | packages.NeedDeps | packages.NeedImports | packages.NeedName,
	}
	pkgs, err := packages.Load(cfg, stdPkgs...)
	if err != nil {
		t.Fatalf("packages.Load(std...): %v", err)
	}
	if len(pkgs) == 0 {
		t.Fatal("no packages loaded")
	}

	var totalSymbols, totalTypes, invalidCount, opaqueCount int
	opaqueByReason := map[OpaqueReason]int{}
	invalidSamples := []string{}
	opaqueSamples := map[OpaqueReason][]string{}

	checkType := func(label string, gt types.Type) {
		if gt == nil {
			return
		}
		totalTypes++
		mt := GoToMochi(gt)
		if mt.Kind == KindInvalid {
			invalidCount++
			if len(invalidSamples) < 10 {
				invalidSamples = append(invalidSamples, label)
			}
			return
		}
		visitType(mt, func(child Type) {
			if child.Kind == KindOpaque {
				opaqueCount++
				opaqueByReason[child.OpaqueReason]++
				if child.OpaqueReason == OpaqueUnknown && child.GoType == "" {
					t.Errorf("%s: KindOpaque with empty GoType", label)
				}
				if len(opaqueSamples[child.OpaqueReason]) < 5 {
					opaqueSamples[child.OpaqueReason] = append(opaqueSamples[child.OpaqueReason], label)
				}
			}
		})
	}

	for _, p := range pkgs {
		if p.Types == nil {
			continue
		}
		scope := p.Types.Scope()
		for _, name := range scope.Names() {
			if !ast.IsExported(name) {
				continue
			}
			obj := scope.Lookup(name)
			if obj == nil {
				continue
			}
			totalSymbols++
			switch o := obj.(type) {
			case *types.Func:
				if sig, ok := o.Type().(*types.Signature); ok {
					checkType(p.PkgPath+"."+name+" (func)", sig)
				}
			case *types.TypeName:
				checkType(p.PkgPath+"."+name+" (type)", o.Type())
			case *types.Var:
				checkType(p.PkgPath+"."+name+" (var)", o.Type())
			case *types.Const:
				checkType(p.PkgPath+"."+name+" (const)", o.Type())
			}
		}
	}

	if invalidCount > 0 {
		t.Errorf("%d KindInvalid results out of %d; samples: %v", invalidCount, totalTypes, invalidSamples)
	}

	density := 0.0
	if totalTypes > 0 {
		density = 100.0 * float64(opaqueCount) / float64(totalTypes)
	}
	unexpectedOpaque := 0
	for reason, cnt := range opaqueByReason {
		if !expectedOpaqueReasons[reason] {
			unexpectedOpaque += cnt
		}
	}
	unexpectedDensity := 0.0
	if totalTypes > 0 {
		unexpectedDensity = 100.0 * float64(unexpectedOpaque) / float64(totalTypes)
	}
	t.Logf("full stdlib soak: %d packages, %d exported symbols, %d type refs", len(pkgs), totalSymbols, totalTypes)
	t.Logf("opaque: %d (%0.2f%% raw density, %0.2f%% unexpected)", opaqueCount, density, unexpectedDensity)
	for reason, cnt := range opaqueByReason {
		tag := "unexpected"
		if expectedOpaqueReasons[reason] {
			tag = "expected"
		}
		t.Logf("  %s [%s]: %d  samples=%s", reason, tag, cnt, strings.Join(opaqueSamples[reason], ", "))
	}
	if unexpectedDensity >= 10.0 {
		t.Errorf("unexpected opaque density %0.2f%% exceeds the MEP-44 §6 10%% bar", unexpectedDensity)
	}
}

// visitType is a depth-first walker over Type structure used by the
// soak test to observe nested KindOpaque values.
func visitType(t Type, fn func(Type)) {
	fn(t)
	if t.Elem != nil {
		visitType(*t.Elem, fn)
	}
	if t.Key != nil {
		visitType(*t.Key, fn)
	}
	for _, f := range t.Fields {
		visitType(f.Type, fn)
	}
	for _, p := range t.Params {
		visitType(p, fn)
	}
	for _, r := range t.Results {
		visitType(r, fn)
	}
	for _, a := range t.TypeArgs {
		visitType(a, fn)
	}
	// Methods are not walked: the soak measures the surface presented
	// to Mochi, and method signatures are already exercised when the
	// declaring type is reached via its top-level reference.
}
