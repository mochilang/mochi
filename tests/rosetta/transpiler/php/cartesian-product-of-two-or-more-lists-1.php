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
function cart2($a, $b) {
  global $llStr, $main;
  $p = [];
  foreach ($a as $x) {
  foreach ($b as $y) {
  $p = array_merge($p, [[$x, $y]]);
};
};
  return $p;
}
function llStr($lst) {
  global $cart2, $main;
  $s = '[';
  $i = 0;
  while ($i < count($lst)) {
  $row = $lst[$i];
  $s = $s . '[';
  $j = 0;
  while ($j < count($row)) {
  $s = $s . _str($row[$j]);
  if ($j < count($row) - 1) {
  $s = $s . ' ';
}
  $j = $j + 1;
};
  $s = $s . ']';
  if ($i < count($lst) - 1) {
  $s = $s . ' ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
}
function main() {
  global $cart2, $llStr;
  echo rtrim(llStr(cart2([1, 2], [3, 4]))), PHP_EOL;
  echo rtrim(llStr(cart2([3, 4], [1, 2]))), PHP_EOL;
  echo rtrim(llStr(cart2([1, 2], []))), PHP_EOL;
  echo rtrim(llStr(cart2([], [1, 2]))), PHP_EOL;
}
main();
