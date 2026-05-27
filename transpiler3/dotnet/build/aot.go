package build

// packAot publishes a NativeAOT ahead-of-time compiled binary to outDir.
// This is a stub in Phase 0; the implementation lands in Phase 1.
func packAot(_, _, _, _ string) error {
	// TODO(Phase 1): run `dotnet publish -r RID -p:PublishAot=true -o outDir`
	return nil
}
