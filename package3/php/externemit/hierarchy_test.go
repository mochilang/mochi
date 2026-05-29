package externemit

import (
	"strings"
	"testing"

	"mochi/package3/php/reflect"
)

func buildTestSurface() *reflect.ReflectionSurface {
	return &reflect.ReflectionSurface{
		PackageName: "acme/shop",
		Classes: []reflect.ClassSurface{
			{FQCN: `Acme\Shop\AbstractAnimal`, Abstract: true},
			{FQCN: `Acme\Shop\Dog`, ParentFQCN: `Acme\Shop\AbstractAnimal`, InterfaceFQCNs: []string{`Acme\Shop\Runnable`}},
			{FQCN: `Acme\Shop\Cat`, ParentFQCN: `Acme\Shop\AbstractAnimal`},
			{FQCN: `Acme\Shop\GuideDog`, ParentFQCN: `Acme\Shop\Dog`},
			{FQCN: `Acme\Shop\Service`, InterfaceFQCNs: []string{`Acme\Shop\Runnable`, `Acme\Shop\Loggable`}},
			{FQCN: `Acme\Shop\AbstractLogger`, Abstract: true, InterfaceFQCNs: []string{`Acme\Shop\Loggable`}},
		},
		Interfaces: []reflect.InterfaceSurface{
			{FQCN: `Acme\Shop\Runnable`},
			{FQCN: `Acme\Shop\Loggable`},
		},
	}
}

func TestBuildHierarchyClassByFQCN(t *testing.T) {
	h := BuildHierarchy(buildTestSurface())
	if _, ok := h.ClassByFQCN[`Acme\Shop\Dog`]; !ok {
		t.Error("expected Dog in ClassByFQCN")
	}
	if len(h.ClassByFQCN) != 6 {
		t.Errorf("expected 6 classes; got %d", len(h.ClassByFQCN))
	}
}

func TestBuildHierarchyInterfaceByFQCN(t *testing.T) {
	h := BuildHierarchy(buildTestSurface())
	if _, ok := h.InterfaceByFQCN[`Acme\Shop\Runnable`]; !ok {
		t.Error("expected Runnable in InterfaceByFQCN")
	}
	if len(h.InterfaceByFQCN) != 2 {
		t.Errorf("expected 2 interfaces; got %d", len(h.InterfaceByFQCN))
	}
}

func TestBuildHierarchyAbstractClasses(t *testing.T) {
	h := BuildHierarchy(buildTestSurface())
	if !h.AbstractClasses[`Acme\Shop\AbstractAnimal`] {
		t.Error("AbstractAnimal should be abstract")
	}
	if !h.AbstractClasses[`Acme\Shop\AbstractLogger`] {
		t.Error("AbstractLogger should be abstract")
	}
	if h.AbstractClasses[`Acme\Shop\Dog`] {
		t.Error("Dog should not be abstract")
	}
}

func TestBuildHierarchySubclassesOf(t *testing.T) {
	h := BuildHierarchy(buildTestSurface())
	subs := h.SubclassesOf[`Acme\Shop\AbstractAnimal`]
	if len(subs) != 2 {
		t.Errorf("expected 2 direct subclasses of AbstractAnimal; got %d: %v", len(subs), subs)
	}
	dogSubs := h.SubclassesOf[`Acme\Shop\Dog`]
	if len(dogSubs) != 1 || dogSubs[0] != `Acme\Shop\GuideDog` {
		t.Errorf("expected GuideDog as subclass of Dog; got %v", dogSubs)
	}
}

func TestBuildHierarchyImplementorsOf(t *testing.T) {
	h := BuildHierarchy(buildTestSurface())
	impls := h.ImplementorsOf[`Acme\Shop\Runnable`]
	if len(impls) != 2 {
		t.Errorf("expected 2 implementors of Runnable; got %d: %v", len(impls), impls)
	}
}

func TestAllSubclassesTransitive(t *testing.T) {
	h := BuildHierarchy(buildTestSurface())
	all := h.AllSubclasses(`Acme\Shop\AbstractAnimal`)
	// Dog, Cat, GuideDog (GuideDog is transitive via Dog)
	if len(all) != 3 {
		t.Errorf("expected 3 total subclasses; got %d: %v", len(all), all)
	}
	found := map[string]bool{}
	for _, s := range all {
		found[s] = true
	}
	for _, want := range []string{`Acme\Shop\Dog`, `Acme\Shop\Cat`, `Acme\Shop\GuideDog`} {
		if !found[want] {
			t.Errorf("expected %q in AllSubclasses result", want)
		}
	}
}

func TestAllSubclassesNoCycles(t *testing.T) {
	// Surface with no subclasses.
	surface := &reflect.ReflectionSurface{
		Classes: []reflect.ClassSurface{
			{FQCN: `A\B`, Abstract: true},
		},
	}
	h := BuildHierarchy(surface)
	result := h.AllSubclasses(`A\B`)
	if len(result) != 0 {
		t.Errorf("expected no subclasses; got %v", result)
	}
}

func TestConcreteImplementors(t *testing.T) {
	h := BuildHierarchy(buildTestSurface())
	// Runnable implementors: Dog (concrete), Service (concrete)
	impls := h.ConcreteImplementors(`Acme\Shop\Runnable`)
	if len(impls) != 2 {
		t.Errorf("expected 2 concrete implementors of Runnable; got %d: %v", len(impls), impls)
	}
}

func TestConcreteImplementorsExcludesAbstract(t *testing.T) {
	h := BuildHierarchy(buildTestSurface())
	// Loggable implementors: Service (concrete), AbstractLogger (abstract)
	impls := h.ConcreteImplementors(`Acme\Shop\Loggable`)
	if len(impls) != 1 || impls[0] != `Acme\Shop\Service` {
		t.Errorf("expected only Service; got %v", impls)
	}
}

func TestInterfacesOf(t *testing.T) {
	h := BuildHierarchy(buildTestSurface())
	ifaces := h.InterfacesOf(`Acme\Shop\Dog`)
	if len(ifaces) != 1 || ifaces[0] != `Acme\Shop\Runnable` {
		t.Errorf("expected [Runnable]; got %v", ifaces)
	}
}

func TestInterfacesOfUnknownClass(t *testing.T) {
	h := BuildHierarchy(buildTestSurface())
	ifaces := h.InterfacesOf(`No\Such\Class`)
	if ifaces != nil {
		t.Errorf("expected nil for unknown class; got %v", ifaces)
	}
}

func TestEmitAbstractBridgeAbstractClass(t *testing.T) {
	result := EmitAbstractBridge(buildTestSurface())
	if result == nil {
		t.Fatal("expected non-nil result")
	}
	src := result.MochiSource
	if !strings.Contains(src, "extern type") {
		t.Errorf("expected extern type declaration; got:\n%s", src)
	}
	// AbstractAnimal should appear with concrete subclasses annotation.
	if !strings.Contains(src, "Abstract base; concrete subclasses:") {
		t.Errorf("expected concrete subclasses comment; got:\n%s", src)
	}
}

func TestEmitAbstractBridgeInterface(t *testing.T) {
	result := EmitAbstractBridge(buildTestSurface())
	src := result.MochiSource
	// Runnable interface should appear with implementors comment.
	if !strings.Contains(src, "Interface; concrete implementors:") {
		t.Errorf("expected implementors comment; got:\n%s", src)
	}
}

func TestEmitAbstractBridgeEmptySurface(t *testing.T) {
	surface := &reflect.ReflectionSurface{PackageName: "empty/pkg"}
	result := EmitAbstractBridge(surface)
	if result == nil {
		t.Fatal("expected non-nil result for empty surface")
	}
	if result.MochiSource != "" {
		t.Errorf("expected empty output for empty surface; got:\n%s", result.MochiSource)
	}
}

func TestEmitAbstractBridgeSkipsConcreteClasses(t *testing.T) {
	surface := &reflect.ReflectionSurface{
		PackageName: "x/y",
		Classes: []reflect.ClassSurface{
			{FQCN: `X\Y\Concrete`},
		},
	}
	result := EmitAbstractBridge(surface)
	if strings.Contains(result.MochiSource, "extern type") {
		t.Errorf("expected no extern type for concrete-only surface; got:\n%s", result.MochiSource)
	}
}
