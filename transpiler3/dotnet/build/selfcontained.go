package build

// packSelfContained publishes a self-contained .NET application to outDir.
// This is a stub in Phase 0; the implementation lands in Phase 1.
func packSelfContained(_, _, _, _ string) error {
	// TODO(Phase 1): run `dotnet publish --self-contained true -r RID -o outDir`
	return nil
}
