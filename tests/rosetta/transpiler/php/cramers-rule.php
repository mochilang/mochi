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
function det($m) {
  global $v, $d, $x, $i, $mc, $s, $j;
  $n = count($m);
  if ($n == 1) {
  return $m[0][0];
}
  $total = 0.0;
  $sign = 1.0;
  $c = 0;
  while ($c < $n) {
  $sub = [];
  $r = 1;
  while ($r < $n) {
  $row = [];
  $cc = 0;
  while ($cc < $n) {
  if ($cc != $c) {
  $row = array_merge($row, [$m[$r][$cc]]);
}
  $cc = $cc + 1;
};
  $sub = array_merge($sub, [$row]);
  $r = $r + 1;
};
  $total = $total + $sign * $m[0][$c] * det($sub);
  $sign = $sign * (-1.0);
  $c = $c + 1;
};
  return $total;
}
function replaceCol($m, $col, $v) {
  global $d, $x, $i, $mc, $s, $j;
  $res = [];
  $r = 0;
  while ($r < count($m)) {
  $row = [];
  $c = 0;
  while ($c < count($m[$r])) {
  if ($c == $col) {
  $row = array_merge($row, [$v[$r]]);
} else {
  $row = array_merge($row, [$m[$r][$c]]);
}
  $c = $c + 1;
};
  $res = array_merge($res, [$row]);
  $r = $r + 1;
};
  return $res;
}
$m = [[2.0, -1.0, 5.0, 1.0], [3.0, 2.0, 2.0, -6.0], [1.0, 3.0, 3.0, -1.0], [5.0, -2.0, -3.0, 3.0]];
$v = [-3.0, -32.0, -47.0, 49.0];
$d = det($m);
$x = [];
$i = 0;
while ($i < count($v)) {
  $mc = replaceCol($m, $i, $v);
  $x = array_merge($x, [det($mc) / $d]);
  $i = $i + 1;
}
$s = '[';
$j = 0;
while ($j < count($x)) {
  $s = $s . _str($x[$j]);
  if ($j < count($x) - 1) {
  $s = $s . ' ';
}
  $j = $j + 1;
}
$s = $s . ']';
echo rtrim($s), PHP_EOL;
