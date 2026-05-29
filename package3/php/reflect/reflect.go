// Package reflect implements the PHP Reflection API surface extractor for
// the MEP-75 PHP bridge. It invokes a PHP CLI script (reflect.php) via
// exec.Command and parses the emitted JSON surface document. Phase 0 ships
// the package stub; the full implementation arrives in phase 3.
package reflect
