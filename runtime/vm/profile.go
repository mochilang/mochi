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
	const threshold = 0.95
	for i := range fn.Code {
		ins := &fn.Code[i]
		prof := fn.Profile[i]
		if prof.Exec == 0 {
			continue
		}
		intPct := float64(prof.IntOps) / float64(prof.Exec)
		floatPct := float64(prof.FloatOps) / float64(prof.Exec)
		if intPct >= threshold {
			ins.Op = specializeInt(ins.Op)
		} else if floatPct >= threshold {
			ins.Op = specializeFloat(ins.Op)
		}
	}
}

func specializeInt(op Op) Op {
	switch op {
	case OpAdd:
		return OpAddInt
	case OpSub:
		return OpSubInt
	case OpMul:
		return OpMulInt
	case OpDiv:
		return OpDivInt
	case OpMod:
		return OpModInt
	case OpEqual:
		return OpEqualInt
	case OpLess:
		return OpLessInt
	case OpLessEq:
		return OpLessEqInt
	case OpNeg:
		return OpNegInt
	}
	return op
}

func specializeFloat(op Op) Op {
	switch op {
	case OpAdd:
		return OpAddFloat
	case OpSub:
		return OpSubFloat
	case OpMul:
		return OpMulFloat
	case OpDiv:
		return OpDivFloat
	case OpMod:
		return OpModFloat
	case OpEqual:
		return OpEqualFloat
	case OpLess:
		return OpLessFloat
	case OpLessEq:
		return OpLessEqFloat
	case OpNeg:
		return OpNegFloat
	}
	return op
}
