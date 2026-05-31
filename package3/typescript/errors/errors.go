// Package errors carries the bridge's SkipReason, SkipReport, and BridgeError
// types. Lands in MEP-72 Phase 0.
package errors

import "fmt"

// SkipReason classifies why a TypeScript export was not bridged.
type SkipReason int

const (
	SkipUnknown         SkipReason = iota
	SkipGeneric                    // unbound TypeScript generic parameter
	SkipConditional                // conditional types (T extends U ? A : B)
	SkipMappedType                 // mapped types ({ [K in keyof T]: U })
	SkipTemplateLiteral            // template literal types (`${string}foo`)
	SkipIntersection               // intersection types (A & B) with >2 members
	SkipSymbol                     // symbol types
	SkipUnknownType                // `unknown` return type
	SkipAnyType                    // `any` return type
	SkipNeverType                  // `never`
	SkipFunction                   // complex function overloads (>3 signatures)
	SkipInternal                   // @internal JSDoc tag
	SkipDeprecated                 // @deprecated JSDoc tag
	SkipBrowser                    // browser-only API (requires DOM)
	SkipNodeBuiltin                // Node.js built-in module (fs, path, etc.)
)

func (s SkipReason) String() string {
	switch s {
	case SkipUnknown:
		return "SkipUnknown"
	case SkipGeneric:
		return "SkipGeneric"
	case SkipConditional:
		return "SkipConditional"
	case SkipMappedType:
		return "SkipMappedType"
	case SkipTemplateLiteral:
		return "SkipTemplateLiteral"
	case SkipIntersection:
		return "SkipIntersection"
	case SkipSymbol:
		return "SkipSymbol"
	case SkipUnknownType:
		return "SkipUnknownType"
	case SkipAnyType:
		return "SkipAnyType"
	case SkipNeverType:
		return "SkipNeverType"
	case SkipFunction:
		return "SkipFunction"
	case SkipInternal:
		return "SkipInternal"
	case SkipDeprecated:
		return "SkipDeprecated"
	case SkipBrowser:
		return "SkipBrowser"
	case SkipNodeBuiltin:
		return "SkipNodeBuiltin"
	default:
		return fmt.Sprintf("SkipReason(%d)", int(s))
	}
}

// SkipReport records one item that was not bridged.
type SkipReport struct {
	ItemPath string
	Reason   SkipReason
	Detail   string
}

// BridgeError is a structured error from the TypeScript bridge pipeline.
type BridgeError struct {
	Phase   string
	Item    string
	Message string
}

func (e *BridgeError) Error() string {
	return fmt.Sprintf("ts bridge [%s] %s: %s", e.Phase, e.Item, e.Message)
}
