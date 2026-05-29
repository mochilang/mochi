// Package lower translates an aotir.Program into a pysrc.Module.
//
// Entry point: Lower(prog, colours, moduleName) → *pysrc.Module.
//
// Phase 1 covers the subset used by phase01-hello fixtures: print
// for string/int/bool, let-bound integers, and a single Main function
// lowered to a top-level `def main() -> None:` plus `if __name__ == "__main__":` guard.
package lower
