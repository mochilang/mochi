## Packages and Imports

Mochi groups related files into **packages**. Every directory forms a package and all `.mochi` files within it share the same namespace. Start a file with a `package` declaration to name that package:

```mochi
package mathutils

export fun add(a: int, b: int): int {
  return a + b
}
```

Names are private to the package unless marked with `export`. Other code can access exported definitions using the `import` statement:

```mochi
import "mathutils"

print(mathutils.add(2, 3))
```

You can provide an alias with `as`:

```mochi
import "mathutils" as mu
print(mu.add(2, 3))
```

If `as` is omitted, the alias defaults to the last segment of the import path.

Paths starting with `./` or `../` are resolved relative to the importing file.
They may refer to a single file or a directory of `.mochi` files. When
referencing a file, the `.mochi` extension is optional.

```mochi
import "./local/constants"
print(constants.pi())
```
