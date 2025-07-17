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

var helperQuery = `function _query($src, $joins, $opts) {
    $items = [];
    foreach ($src as $v) { $items[] = [$v]; }
    foreach ($joins as $j) {
        $joined = [];
        $jitems = $j['items'] ?? [];
        if (($j['right'] ?? false) && ($j['left'] ?? false)) {
            $matched = array_fill(0, count($jitems), false);
            foreach ($items as $left) {
                $m = false;
                foreach ($jitems as $ri => $right) {
                    $keep = true;
                    if (isset($j['on'])) {
                        $args = $left; $args[] = $right;
                        $keep = $j['on'](...$args);
                    }
                    if (!$keep) continue;
                    $m = true; $matched[$ri] = true;
                    $row = $left; $row[] = $right;
                    $joined[] = $row;
                }
                if (!$m) { $row = $left; $row[] = null; $joined[] = $row; }
            }
            foreach ($jitems as $ri => $right) {
                if (!$matched[$ri]) {
                    $undef = count($items) > 0 ? array_fill(0, count($items[0]), null) : [];
                    $row = $undef; $row[] = $right; $joined[] = $row;
                }
            }
        } elseif (($j['right'] ?? false)) {
            foreach ($jitems as $right) {
                $m = false;
                foreach ($items as $left) {
                    $keep = true;
                    if (isset($j['on'])) {
                        $args = $left; $args[] = $right;
                        $keep = $j['on'](...$args);
                    }
                    if (!$keep) continue;
                    $m = true; $row = $left; $row[] = $right; $joined[] = $row;
                }
                if (!$m) { $undef = count($items) > 0 ? array_fill(0, count($items[0]), null) : []; $row = $undef; $row[] = $right; $joined[] = $row; }
            }
        } else {
            foreach ($items as $left) {
                $m = false;
                foreach ($jitems as $right) {
                    $keep = true;
                    if (isset($j['on'])) {
                        $args = $left; $args[] = $right;
                        $keep = $j['on'](...$args);
                    }
                    if (!$keep) continue;
                    $m = true; $row = $left; $row[] = $right; $joined[] = $row;
                }
                if (($j['left'] ?? false) && !$m) { $row = $left; $row[] = null; $joined[] = $row; }
            }
        }
        $items = $joined;
    }
    if (isset($opts['where'])) {
        $fn = $opts['where'];
        $items = array_values(array_filter($items, fn($r) => $fn(...$r)));
    }
    if (isset($opts['sortKey'])) {
        $sk = $opts['sortKey'];
        usort($items, function($a,$b) use($sk) {
            $ak = $sk(...$a); $bk = $sk(...$b);
            if (is_array($ak) || is_object($ak)) $ak = json_encode($ak);
            if (is_array($bk) || is_object($bk)) $bk = json_encode($bk);
            return $ak <=> $bk;
        });
    }
    if (isset($opts['skip'])) {
        $n = $opts['skip']; if ($n < 0) $n = 0; $items = array_slice($items, $n);
    }
    if (isset($opts['take'])) {
        $n = $opts['take']; if ($n < 0) $n = 0; $items = array_slice($items, 0, $n);
    }
    $res = [];
    $sel = $opts['select'];
    foreach ($items as $r) { $res[] = $sel(...$r); }
    return $res;
}`

var helperGroupBy = `function _group_by($src, $keyfn) {
    $groups = [];
    $order = [];
    foreach ($src as $it) {
        $key = is_array($it) ? $keyfn(...$it) : $keyfn($it);
        if (is_array($key)) $key = (object)$key;
        $ks = json_encode($key);
        if (!isset($groups[$ks])) { $groups[$ks] = ['key'=>$key,'items'=>[]]; $order[] = $ks; }
        $groups[$ks]['items'][] = $it;
    }
    $res = [];
    foreach ($order as $k) { $res[] = $groups[$k]; }
    return $res;
}`

var helperAvg = `function _avg($v) {
    if (is_array($v) && array_key_exists('items', $v)) {
        $v = $v['items'];
    } elseif (is_object($v) && property_exists($v, 'items')) {
        $v = $v->items;
    }
    if (!is_array($v)) {
        throw new Exception('avg() expects list or group');
    }
    if (!$v) return 0;
    $sum = 0;
    foreach ($v as $it) {
        if (is_int($it) || is_float($it)) {
            $sum += $it;
        } else {
            throw new Exception('avg() expects numbers');
        }
    }
    return $sum / count($v);
}`

var helperMap = map[string]string{
	"_load":     helperLoad,
	"_save":     helperSave,
	"_query":    helperQuery,
	"_group_by": helperGroupBy,
	"_avg":      helperAvg,
}
