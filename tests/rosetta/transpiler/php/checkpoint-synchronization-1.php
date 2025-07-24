<?php
ini_set('memory_limit', '-1');
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
$partList = ['A', 'B', 'C', 'D'];
$nAssemblies = 3;
for ($cycle = 1; $cycle < ($nAssemblies + 1); $cycle++) {
  echo rtrim('begin assembly cycle ' . _str($cycle)), PHP_EOL;
  foreach ($partList as $p) {
  echo rtrim($p . ' worker begins part'), PHP_EOL;
};
  foreach ($partList as $p) {
  echo rtrim($p . ' worker completes part'), PHP_EOL;
};
  echo rtrim('assemble.  cycle ' . _str($cycle) . ' complete'), PHP_EOL;
}
