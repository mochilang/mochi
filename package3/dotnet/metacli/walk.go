package metacli

// WalkTypes calls fn for each SurfaceType in the ApiSurface. If fn returns
// false the walk stops early.
func (s *ApiSurface) WalkTypes(fn func(SurfaceType) bool) {
	for _, t := range s.Types {
		if !fn(t) {
			return
		}
	}
}

// WalkMethods calls fn for each SurfaceMethod on every SurfaceType in the
// ApiSurface. The SurfaceType is passed alongside the method so callers can
// resolve the owning type. If fn returns false the walk stops early.
func (s *ApiSurface) WalkMethods(fn func(t SurfaceType, m SurfaceMethod) bool) {
	for _, t := range s.Types {
		for _, m := range t.Methods {
			if !fn(t, m) {
				return
			}
		}
	}
}

// WalkProps calls fn for each SurfaceProp on every SurfaceType in the
// ApiSurface. If fn returns false the walk stops early.
func (s *ApiSurface) WalkProps(fn func(t SurfaceType, p SurfaceProp) bool) {
	for _, t := range s.Types {
		for _, p := range t.Properties {
			if !fn(t, p) {
				return
			}
		}
	}
}
