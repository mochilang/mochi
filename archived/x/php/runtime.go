//go:build archived

package phpcode

// Runtime helpers emitted by the PHP compiler.

const helperQuery = "function _query($src, $joins, $opts) {\n" +
	"    $items = array_map(fn($v) => [$v], $src);\n" +
	"    foreach ($joins as $j) {\n" +
	"        $joined = [];\n" +
	"        if (!empty($j['right']) && !empty($j['left'])) {\n" +
	"            $matched = array_fill(0, count($j['items']), false);\n" +
	"            foreach ($items as $left) {\n" +
	"                $m = false;\n" +
	"                foreach ($j['items'] as $ri => $right) {\n" +
	"                    $keep = true;\n" +
	"                    if (isset($j['on'])) { $args = array_merge($left, [$right]); $keep = $j['on'](...$args); }\n" +
	"                    if (!$keep) continue;\n" +
	"                    $m = true; $matched[$ri] = true;\n" +
	"                    $joined[] = array_merge($left, [$right]);\n" +
	"                }\n" +
	"                if (!$m) { $joined[] = array_merge($left, [null]); }\n" +
	"            }\n" +
	"            foreach ($j['items'] as $ri => $right) {\n" +
	"                if (!$matched[$ri]) {\n" +
	"                    $undef = count($items) > 0 ? array_fill(0, count($items[0]), null) : [];\n" +
	"                    $joined[] = array_merge($undef, [$right]);\n" +
	"                }\n" +
	"            }\n" +
	"        } elseif (!empty($j['right'])) {\n" +
	"            foreach ($j['items'] as $right) {\n" +
	"                $m = false;\n" +
	"                foreach ($items as $left) {\n" +
	"                    $keep = true;\n" +
	"                    if (isset($j['on'])) { $args = array_merge($left, [$right]); $keep = $j['on'](...$args); }\n" +
	"                    if (!$keep) continue;\n" +
	"                    $m = true; $joined[] = array_merge($left, [$right]);\n" +
	"                }\n" +
	"                if (!$m) {\n" +
	"                    $undef = count($items) > 0 ? array_fill(0, count($items[0]), null) : [];\n" +
	"                    $joined[] = array_merge($undef, [$right]);\n" +
	"                }\n" +
	"            }\n" +
	"        } else {\n" +
	"            foreach ($items as $left) {\n" +
	"                $m = false;\n" +
	"                foreach ($j['items'] as $right) {\n" +
	"                    $keep = true;\n" +
	"                    if (isset($j['on'])) { $args = array_merge($left, [$right]); $keep = $j['on'](...$args); }\n" +
	"                    if (!$keep) continue;\n" +
	"                    $m = true; $joined[] = array_merge($left, [$right]);\n" +
	"                }\n" +
	"                if (!empty($j['left']) && !$m) { $joined[] = array_merge($left, [null]); }\n" +
	"            }\n" +
	"        }\n" +
	"        $items = $joined;\n" +
	"    }\n" +
	"    if (isset($opts['where'])) {\n" +
	"        $filtered = [];\n" +
	"        foreach ($items as $r) { if ($opts['where'](...$r)) $filtered[] = $r; }\n" +
	"        $items = $filtered;\n" +
	"    }\n" +
	"    if (isset($opts['sortKey'])) {\n" +
	"        $pairs = [];\n" +
	"        foreach ($items as $it) { $pairs[] = ['item' => $it, 'key' => $opts['sortKey'](...$it)]; }\n" +
	"        usort($pairs, function($a, $b) {\n" +
	"            $ak = $a['key']; $bk = $b['key'];\n" +
	"            if (is_int($ak) && is_int($bk)) return $ak <=> $bk;\n" +
	"            if (is_string($ak) && is_string($bk)) return $ak <=> $bk;\n" +
	"            return strcmp(strval($ak), strval($bk));\n" +
	"        });\n" +
	"        $items = array_map(fn($p) => $p['item'], $pairs);\n" +
	"    }\n" +
	"    if (array_key_exists('skip', $opts)) {\n" +
	"        $n = $opts['skip'];\n" +
	"        $items = $n < count($items) ? array_slice($items, $n) : [];\n" +
	"    }\n" +
	"    if (array_key_exists('take', $opts)) {\n" +
	"        $n = $opts['take'];\n" +
	"        if ($n < count($items)) $items = array_slice($items, 0, $n);\n" +
	"    }\n" +
	"    $res = [];\n" +
	"    foreach ($items as $r) { $res[] = $opts['select'](...$r); }\n" +
	"    return $res;\n" +
	"}\n"

const helperGroupClass = "class _Group {\n" +
	"    public $key;\n" +
	"    public $Items;\n" +
	"    function __construct($key) { $this->key = $key; $this->Items = []; }\n" +
	"}\n"

const helperGroupBy = "function _group_by($src, $keyfn) {\n" +
	"    $groups = [];\n" +
	"    $order = [];\n" +
	"    foreach ($src as $it) {\n" +
	"        $key = $keyfn($it);\n" +
	"        if (is_array($key)) { $key = (object)$key; }\n" +
	"        $ks = is_object($key) ? json_encode($key) : strval($key);\n" +
	"        if (!isset($groups[$ks])) {\n" +
	"            $groups[$ks] = new _Group($key);\n" +
	"            $order[] = $ks;\n" +
	"        }\n" +
	"        $groups[$ks]->Items[] = $it;\n" +
	"    }\n" +
	"    $res = [];\n" +
	"    foreach ($order as $ks) { $res[] = $groups[$ks]; }\n" +
	"    return $res;\n" +
	"}\n"

const helperLoadJSON = "function _load_json($path) {\n" +
	"    $f = ($path === '' || $path === '-') ? fopen('php://stdin', 'r') : fopen($path, 'r');\n" +
	"    if (!$f) { throw new Exception('cannot open ' . $path); }\n" +
	"    $data = stream_get_contents($f);\n" +
	"    if ($path !== '' && $path !== '-') fclose($f);\n" +
	"    $val = json_decode($data);\n" +
	"    if ($val === null) return [];\n" +
	"    if (array_keys($val) !== range(0, count($val) - 1)) { return [$val]; }\n" +
	"    return $val;\n" +
	"}\n"

const helperSaveJSON = "function _save_json($rows, $path) {\n" +
	"    $out = json_encode($rows);\n" +
	"    if ($path === '' || $path === '-') { fwrite(STDOUT, $out . PHP_EOL); } else { file_put_contents($path, $out); }\n" +
	"}\n"

const helperPrint = "function _print(...$args) {\n" +
	"    $parts = [];\n" +
	"    foreach ($args as $a) {\n" +
	"        if (is_null($a)) { $parts[] = '<nil>'; }\n" +
	"        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }\n" +
	"    }\n" +
	"    echo implode(' ', $parts), PHP_EOL;\n" +
	"}\n"

const helperGenText = "function _gen_text($prompt, $model, $params) {\n" +
	"    return $prompt;\n" +
	"}\n"

const helperGenEmbed = "function _gen_embed($text, $model, $params) {\n" +
	"    $out = [];\n" +
	"    for ($i = 0; $i < strlen($text); $i++) { $out[] = ord($text[$i]); }\n" +
	"    return $out;\n" +
	"}\n"

const helperGenStruct = "function _gen_struct($prompt, $model, $params) {\n" +
	"    $data = json_decode($prompt, true);\n" +
	"    return is_array($data) ? $data : [];\n" +
	"}\n"

const helperFetch = "function _fetch($url, $opts = null) {\n" +
	"    $args = ['-s'];\n" +
	"    $method = $opts['method'] ?? 'GET';\n" +
	"    $args[] = '-X'; $args[] = $method;\n" +
	"    if (isset($opts['headers'])) {\n" +
	"        foreach ($opts['headers'] as $k => $v) { $args[] = '-H'; $args[] = $k . ': ' . strval($v); }\n" +
	"    }\n" +
	"    if (isset($opts['query'])) {\n" +
	"        $qs = http_build_query($opts['query']);\n" +
	"        $sep = strpos($url, '?') !== false ? '&' : '?';\n" +
	"        $url .= $sep . $qs;\n" +
	"    }\n" +
	"    if ($opts !== null && array_key_exists('body', $opts)) { $args[] = '-d'; $args[] = json_encode($opts['body']); }\n" +
	"    if (isset($opts['timeout'])) { $args[] = '--max-time'; $args[] = strval($opts['timeout']); }\n" +
	"    $args[] = $url;\n" +
	"    $escaped = array_map('escapeshellarg', $args);\n" +
	"    $cmd = 'curl ' . implode(' ', $escaped);\n" +
	"    $data = shell_exec($cmd);\n" +
	"    return json_decode($data);\n" +
	"}\n"

var helperMap = map[string]string{
	"_query":      helperQuery,
	"_group":      helperGroupClass,
	"_group_by":   helperGroupBy,
	"_fetch":      helperFetch,
	"_load_json":  helperLoadJSON,
	"_save_json":  helperSaveJSON,
	"_gen_text":   helperGenText,
	"_gen_embed":  helperGenEmbed,
	"_gen_struct": helperGenStruct,
	"_print":      helperPrint,
}
