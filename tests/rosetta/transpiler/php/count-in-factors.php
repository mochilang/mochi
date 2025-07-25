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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function show($n) {
  if ($n == 1) {
  echo rtrim('1: 1'), PHP_EOL;
  return;
}
  $out = _str($n) . ': ';
  $x = '';
  $m = $n;
  $f = 2;
  while ($m != 1) {
  if ($m % $f == 0) {
  $out = $out . $x . _str($f);
  $x = '×';
  $m = intval((_intdiv($m, $f)));
} else {
  $f = $f + 1;
}
};
  echo rtrim($out), PHP_EOL;
}
show(1);
for ($i = 2; $i < 10; $i++) {
  show($i);
}
echo rtrim('...'), PHP_EOL;
for ($i = 2144; $i < 2155; $i++) {
  show($i);
}
echo rtrim('...'), PHP_EOL;
for ($i = 9987; $i < 10000; $i++) {
  show($i);
}
