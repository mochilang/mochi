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
$n = 15;
$t = [];
for ($_ = 0; $_ < ($n + 2); $_++) {
  $t = array_merge($t, [0]);
}
$t[1] = 1;
for ($i = 1; $i < ($n + 1); $i++) {
  $j = $i;
  while ($j > 1) {
  $t[$j] = $t[$j] + $t[$j - 1];
  $j = $j - 1;
};
  $t[intval(($i + 1))] = $t[$i];
  $j = $i + 1;
  while ($j > 1) {
  $t[$j] = $t[$j] + $t[$j - 1];
  $j = $j - 1;
};
  $cat = $t[$i + 1] - $t[$i];
  if ($i < 10) {
  echo rtrim(' ' . _str($i) . ' : ' . _str($cat)), PHP_EOL;
} else {
  echo rtrim(_str($i) . ' : ' . _str($cat)), PHP_EOL;
}
}
