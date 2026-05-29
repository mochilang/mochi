package wrapper

import (
	"fmt"
	"sort"
	"strings"

	"mochi/package3/python/errors"
	"mochi/package3/python/stubs"
	"mochi/package3/python/typemap"
)

// Synthesise consumes a typed ModuleSurface for one PyPI package and emits a
// Wrapper containing the Python source + .pyi stub the Mochi side imports.
//
// The synthesiser is deterministic: items are walked in the surface's
// declaration order and the rendered files only contain items the type mapper
// accepted. Refused items appear in Wrapper.Skipped and the generated
// SKIPPED.txt block. The wrapper is silent on items whose source name begins
// with an underscore (they are private and not part of the bridged surface);
// the only exception is dunder methods inside Protocols, which the Phase 4
// mapper already classifies as Skipped before this phase sees them.
func Synthesise(pkg string, surface *stubs.ModuleSurface, opts Options) (*Wrapper, error) {
	if pkg == "" {
		return nil, fmt.Errorf("wrapper: package name is empty")
	}
	if surface == nil {
		return nil, fmt.Errorf("wrapper: surface is nil")
	}
	if !isValidPyIdent(pkg) {
		return nil, fmt.Errorf("wrapper: package name %q is not a valid Python identifier", pkg)
	}

	w := &Wrapper{
		Package: pkg,
		Module:  pkg + "_externs",
		Loop:    opts.Loop,
	}
	mapper := &typemap.Mapper{AllowPartial: opts.AllowPartial}

	for _, c := range surface.Classes {
		if isPrivate(c.Name) {
			w.Skipped = append(w.Skipped, errors.SkipReport{
				ItemPath: pkg + "." + c.Name,
				Reason:   errors.SkipPrivateName,
				Detail:   "class " + c.Name + " is private (leading underscore)",
			})
			continue
		}
		dec := mapper.MapClass(c)
		if !dec.OK() {
			r := *dec.Skip
			r.ItemPath = qualify(pkg, r.ItemPath)
			w.Skipped = append(w.Skipped, r)
			continue
		}
		for _, s := range dec.Skipped {
			s.ItemPath = qualify(pkg, s.ItemPath)
			w.Skipped = append(w.Skipped, s)
		}
		kind := ItemRecord
		if dec.Type.Kind == typemap.KindInterface {
			kind = ItemInterface
		}
		w.Items = append(w.Items, Item{
			Name:       c.Name,
			SourceName: pkg + "." + c.Name,
			Kind:       kind,
			Type:       dec.Type,
		})
	}

	for _, fn := range surface.Functions {
		if isPrivate(fn.Name) {
			w.Skipped = append(w.Skipped, errors.SkipReport{
				ItemPath: pkg + "." + fn.Name,
				Reason:   errors.SkipPrivateName,
				Detail:   "function " + fn.Name + " is private (leading underscore)",
			})
			continue
		}
		dec := mapper.MapFunction(fn)
		if !dec.OK() {
			r := *dec.Skip
			r.ItemPath = qualify(pkg, r.ItemPath)
			w.Skipped = append(w.Skipped, r)
			continue
		}
		name := fn.Name
		if fn.IsAsync {
			name = fn.Name + "_sync"
		}
		w.Items = append(w.Items, Item{
			Name:       name,
			SourceName: pkg + "." + fn.Name,
			Kind:       ItemFunc,
			Type:       dec.Type,
			IsAsync:    fn.IsAsync,
			Loop:       opts.Loop,
		})
	}

	for _, c := range surface.Constants {
		if isPrivate(c.Name) {
			w.Skipped = append(w.Skipped, errors.SkipReport{
				ItemPath: pkg + "." + c.Name,
				Reason:   errors.SkipPrivateName,
				Detail:   "constant " + c.Name + " is private (leading underscore)",
			})
			continue
		}
		if c.Type == "" {
			w.Skipped = append(w.Skipped, errors.SkipReport{
				ItemPath: pkg + "." + c.Name,
				Reason:   errors.SkipUnsupportedTypingConstruct,
				Detail:   "constant " + c.Name + " has no annotation",
			})
			continue
		}
		dec := mapper.Map(c.Type)
		if !dec.OK() {
			r := *dec.Skip
			r.ItemPath = pkg + "." + c.Name
			w.Skipped = append(w.Skipped, r)
			continue
		}
		w.Items = append(w.Items, Item{
			Name:       c.Name,
			SourceName: pkg + "." + c.Name,
			Kind:       ItemConstant,
			Type:       dec.Type,
		})
	}

	sort.SliceStable(w.Items, func(i, j int) bool {
		return w.Items[i].SourceName < w.Items[j].SourceName
	})
	sort.SliceStable(w.Skipped, func(i, j int) bool {
		return w.Skipped[i].ItemPath < w.Skipped[j].ItemPath
	})

	w.PySource = renderPy(w)
	w.PYISource = renderPYI(w)
	return w, nil
}

// SkippedText renders the SKIPPED.txt block that ships alongside the wrapper.
// Mirrors errors.SkipReport.String() for every entry in Skipped.
func (w *Wrapper) SkippedText() string {
	if len(w.Skipped) == 0 {
		return "# No items skipped during wrapper synthesis for " + w.Package + ".\n"
	}
	var b strings.Builder
	b.WriteString("# Wrapper synthesis skipped the following items in ")
	b.WriteString(w.Package)
	b.WriteString(".\n\n")
	for _, s := range w.Skipped {
		b.WriteString(s.String())
		b.WriteString("\n")
	}
	return b.String()
}

func qualify(pkg, path string) string {
	if strings.HasPrefix(path, pkg+".") || path == pkg {
		return path
	}
	if path == "" {
		return pkg
	}
	return pkg + "." + path
}

func isPrivate(name string) bool {
	return strings.HasPrefix(name, "_") && !(strings.HasPrefix(name, "__") && strings.HasSuffix(name, "__"))
}

func isValidPyIdent(s string) bool {
	if s == "" {
		return false
	}
	for i, r := range s {
		switch {
		case r == '_' || (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z'):
			// always ok
		case i > 0 && r >= '0' && r <= '9':
			// digits allowed after first char
		default:
			return false
		}
	}
	return true
}
