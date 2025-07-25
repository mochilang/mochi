//go:build ignore
// +build ignore

ch := 'z'          // ch is type rune (an int32 type)
var r rune = 'z'   // r is type rune
var b byte = 'z'   // b is type byte (an uint8 type)
b2 := byte('z')    // equivalent to b
const z = 'z'      // z is untyped, it may be freely assigned or used in any integer expression
b = z
r = z
ch2 := z           // equivalent to ch (type rune)
var i int = z
const c byte = 'z' // c is a typed constant
b = c
r = rune(c)
i = int(c)
b3 := c            // equivalent to b
