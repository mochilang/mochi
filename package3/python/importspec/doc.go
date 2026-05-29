// Package importspec parses the body of the MEP-71 surface form
//
//	import python "<spec>" as <alias>
//
// per MEP-71 §3 "import python grammar". The package recognises five spec
// shapes:
//
//	bare              "requests"               name=requests, source=registry, version=*
//	semver-pinned     "requests@>=2.0,<3.0"    name=requests, version=spec
//	non-default index "torch@torch+https://download.pytorch.org/whl/cu121"
//	VCS               "mypkg@git+https://github.com/user/repo#commit"
//	local path        "mypkg@path+../sibling"
//
// The parser does not consume the alias; that is part of the Mochi grammar
// the MEP-51 parser already handles. importspec.Parse is fed only the
// double-quoted string body. The returned Spec is what Phase 8 (build) and
// Phase 9 (lockfile) consume.
//
// See MEP-71 §3 "Surface keyword" for the normative grammar and §5
// "Lockfile entry" for what each Spec form translates to in mochi.lock.
package importspec
