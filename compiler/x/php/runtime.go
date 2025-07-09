//go:build slow

package phpcode

// helperMap stores snippets of runtime helper functions that the compiler
// can emit if needed. The PHP backend currently has no helpers.
var helperLoad = `function _load($path = null, $opts = []) {
    $fmt = $opts['format'] ?? 'csv';
    if ($path !== null && $path !== '' && $path != '-' && $path[0] !== '/') {
        $path = __DIR__ . '/' . $path;
    }
    if ($fmt === 'yaml') {
        $lines = ($path === null || $path === '' || $path === '-') ?
            file('php://stdin', FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES) :
            file($path, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
        $rows = [];
        $curr = [];
        foreach ($lines as $line) {
            $line = trim($line);
            if (str_starts_with($line, '-')) {
                if ($curr) $rows[] = $curr;
                $curr = [];
                $line = trim(substr($line, 1));
                if ($line !== '') {
                    [$k, $v] = array_map('trim', explode(':', $line, 2));
                    $curr[$k] = is_numeric($v) ? (int)$v : $v;
                }
            } else {
                [$k, $v] = array_map('trim', explode(':', $line, 2));
                $curr[$k] = is_numeric($v) ? (int)$v : $v;
            }
        }
        if ($curr) $rows[] = $curr;
        return $rows;
    }
    return [];
}`

var helperSave = `function _save($rows, $path = null, $opts = []) {
    $fmt = $opts['format'] ?? 'csv';
    if ($fmt === 'jsonl') {
        $out = ($path === null || $path === '' || $path === '-') ? STDOUT : fopen($path, 'w');
        foreach ($rows as $row) {
            if (is_object($row)) $row = (array)$row;
            fwrite($out, json_encode($row) . PHP_EOL);
        }
        if ($out !== STDOUT) fclose($out);
    }
}`

var helperMap = map[string]string{
	"_load": helperLoad,
	"_save": helperSave,
}
