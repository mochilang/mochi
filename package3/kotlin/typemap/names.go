package typemap

import (
	"fmt"
	"strings"
	"unicode"
)

// KotlinToMochiName converts a Kotlin identifier to Mochi snake_case.
//
// Examples:
//
//	"fetchUserById"  -> "fetch_user_by_id"
//	"URL"            -> "url"
//	"getHTTPSStatus" -> "get_https_status"
//
// Rule: a run of consecutive uppercase letters followed by a lowercase letter is treated
// as one word (e.g. "HTTPS" in "getHTTPSStatus"). A single uppercase letter following a
// lowercase letter or digit starts a new word.
func KotlinToMochiName(name string) string {
	if name == "" {
		return ""
	}

	runes := []rune(name)
	var parts []string
	start := 0

	for i := 0; i < len(runes); i++ {
		if i == 0 {
			continue
		}
		cur := runes[i]
		prev := runes[i-1]

		// Transition: lowercase/digit -> uppercase: split before cur
		if (unicode.IsLower(prev) || unicode.IsDigit(prev)) && unicode.IsUpper(cur) {
			parts = append(parts, string(runes[start:i]))
			start = i
			continue
		}

		// Transition: uppercase run followed by lowercase: split before prev
		// e.g. "HTTPSStatus" -> at 'S' of "Status", i=5, prev='S', cur='t'
		// We want split: "HTTPS" | "Status"
		if unicode.IsUpper(prev) && unicode.IsLower(cur) && i-start > 1 {
			parts = append(parts, string(runes[start:i-1]))
			start = i - 1
			continue
		}
	}
	parts = append(parts, string(runes[start:]))

	// Lower-case each part and join with underscores, filtering empty strings
	var sb strings.Builder
	first := true
	for _, p := range parts {
		if p == "" {
			continue
		}
		if !first {
			sb.WriteByte('_')
		}
		sb.WriteString(strings.ToLower(p))
		first = false
	}
	return sb.String()
}

// ClassToExternName returns the simple CamelCase extern type name from a fully qualified class name.
// Nested classes (containing '$') have the separator removed and parts joined.
//
// Examples:
//
//	"com.example.MyClass"         -> "MyClass"
//	"com.example.MyClass$Builder" -> "MyClassBuilder"
func ClassToExternName(fqn string) string {
	// Get the last dot-separated segment
	lastDot := strings.LastIndex(fqn, ".")
	simple := fqn
	if lastDot >= 0 {
		simple = fqn[lastDot+1:]
	}
	// Replace '$' (nested class separator) with nothing — join parts
	simple = strings.ReplaceAll(simple, "$", "")
	return simple
}

// ShimFnName returns the C shim function name for a class method.
// It combines snake_case(className) + "_" + snake_case(fnName).
//
// Examples:
//
//	"OkHttpClient", "newCall" -> "ok_http_client_new_call"
func ShimFnName(className, fnName string) string {
	classSnake := KotlinToMochiName(className)
	fnSnake := KotlinToMochiName(fnName)
	if classSnake == "" {
		return fnSnake
	}
	if fnSnake == "" {
		return classSnake
	}
	return classSnake + "_" + fnSnake
}

// NameRegistry tracks allocated shim names and handles deduplication.
type NameRegistry struct {
	used map[string]int // base name -> next suffix counter
}

// NewNameRegistry creates a new empty NameRegistry.
func NewNameRegistry() *NameRegistry {
	return &NameRegistry{used: make(map[string]int)}
}

// Allocate returns a unique shim function name based on className and fnName.
// If the name is already taken, it appends _2, _3, etc.
func (r *NameRegistry) Allocate(className, fnName string) string {
	base := ShimFnName(className, fnName)
	count := r.used[base]
	r.used[base]++
	if count == 0 {
		return base
	}
	return fmt.Sprintf("%s_%d", base, count+1)
}
