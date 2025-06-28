package vm

import (
	"encoding/json"
	"os"
)

// ProfileEnabled toggles collection of runtime execution profiles.
// When enabled, each function allocates a profile array to record
// instruction usage which can later be saved and fed back into the
// optimizer.
var ProfileEnabled bool

// SaveProfile writes execution profile information for prog to path in JSON
// format. It is a no-op if profiling was not enabled.
func SaveProfile(prog *Program, path string) error {
	if prog == nil {
		return nil
	}
	data := make(map[string][]InstrProfile)
	for i := range prog.Funcs {
		fn := &prog.Funcs[i]
		if len(fn.Profile) == 0 {
			continue
		}
		cp := make([]InstrProfile, len(fn.Profile))
		copy(cp, fn.Profile)
		data[prog.funcName(i)] = cp
	}
	b, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		return err
	}
	return os.WriteFile(path, b, 0644)
}

// LoadProfile loads profiling information from path and attaches it to prog.
func LoadProfile(prog *Program, path string) error {
	b, err := os.ReadFile(path)
	if err != nil {
		return err
	}
	var data map[string][]InstrProfile
	if err := json.Unmarshal(b, &data); err != nil {
		return err
	}
	for i := range prog.Funcs {
		name := prog.funcName(i)
		if prof, ok := data[name]; ok {
			if len(prof) == len(prog.Funcs[i].Code) {
				prog.Funcs[i].Profile = prof
			}
		}
	}
	return nil
}

// ApplyPGO specializes generic numeric operations based on loaded profiles.
func ApplyPGO(prog *Program) {
	for i := range prog.Funcs {
		profileOptimize(&prog.Funcs[i])
	}
}

func profileOptimize(fn *Function) {
	if len(fn.Profile) == 0 {
		return
	}
	for i := range fn.Code {
		ins := &fn.Code[i]
		prof := fn.Profile[i]
		if prof.Exec == 0 {
			continue
		}
		switch ins.Op {
		case OpAdd:
			if prof.IntOps == prof.Exec {
				ins.Op = OpAddInt
			} else if prof.FloatOps == prof.Exec {
				ins.Op = OpAddFloat
			}
		case OpSub:
			if prof.IntOps == prof.Exec {
				ins.Op = OpSubInt
			} else if prof.FloatOps == prof.Exec {
				ins.Op = OpSubFloat
			}
		case OpMul:
			if prof.IntOps == prof.Exec {
				ins.Op = OpMulInt
			} else if prof.FloatOps == prof.Exec {
				ins.Op = OpMulFloat
			}
		case OpDiv:
			if prof.IntOps == prof.Exec {
				ins.Op = OpDivInt
			} else if prof.FloatOps == prof.Exec {
				ins.Op = OpDivFloat
			}
		case OpMod:
			if prof.IntOps == prof.Exec {
				ins.Op = OpModInt
			} else if prof.FloatOps == prof.Exec {
				ins.Op = OpModFloat
			}
		case OpEqual:
			if prof.IntOps == prof.Exec {
				ins.Op = OpEqualInt
			} else if prof.FloatOps == prof.Exec {
				ins.Op = OpEqualFloat
			}
		case OpLess:
			if prof.IntOps == prof.Exec {
				ins.Op = OpLessInt
			} else if prof.FloatOps == prof.Exec {
				ins.Op = OpLessFloat
			}
		case OpLessEq:
			if prof.IntOps == prof.Exec {
				ins.Op = OpLessEqInt
			} else if prof.FloatOps == prof.Exec {
				ins.Op = OpLessEqFloat
			}
		case OpNeg:
			if prof.IntOps == prof.Exec {
				ins.Op = OpNegInt
			} else if prof.FloatOps == prof.Exec {
				ins.Op = OpNegFloat
			}
		}
	}
}
