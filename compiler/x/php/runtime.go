//go:build slow

package phpcode

const helperPrint = "function _print(...$args) {\n" +
	"    $parts = [];\n" +
	"    foreach ($args as $a) {\n" +
	"        if (is_array($a) || is_object($a)) {\n" +
	"            $parts[] = json_encode($a);\n" +
	"        } else {\n" +
	"            $parts[] = strval($a);\n" +
	"        }\n" +
	"    }\n" +
	"    echo implode(' ', $parts), PHP_EOL;\n" +
	"}\n"

var helperMap = map[string]string{
	"_print": helperPrint,
}
