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
function listStr($xs) {
  global $llStr, $cartN, $main;
  $s = '[';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . _str($xs[$i]);
  if ($i < count($xs) - 1) {
  $s = $s . ' ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
}
function llStr($lst) {
  global $listStr, $cartN, $main;
  $s = '[';
  $i = 0;
  while ($i < count($lst)) {
  $s = $s . listStr($lst[$i]);
  if ($i < count($lst) - 1) {
  $s = $s . ' ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
}
function cartN($lists) {
  global $listStr, $llStr, $main;
  if ($lists == null) {
  return [];
}
  $a = $lists;
  if (count($a) == 0) {
  return [[]];
}
  $c = 1;
  foreach ($a as $xs) {
  $c = $c * count($xs);
};
  if ($c == 0) {
  return [];
}
  $res = [];
  $idx = [];
  foreach ($a as $_) {
  $idx = array_merge($idx, [0]);
};
  $n = count($a);
  $count = 0;
  while ($count < $c) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = array_merge($row, [$a[$j][$idx[$j]]]);
  $j = $j + 1;
};
  $res = array_merge($res, [$row]);
  $k = $n - 1;
  while ($k >= 0) {
  $idx[$k] = $idx[$k] + 1;
  if ($idx[$k] < count($a[$k])) {
  break;
}
  $idx[$k] = 0;
  $k = $k - 1;
};
  $count = $count + 1;
};
  return $res;
}
function main() {
  global $listStr, $llStr, $cartN;
  echo rtrim(llStr(cartN([[1, 2], [3, 4]]))), PHP_EOL;
  echo rtrim(llStr(cartN([[3, 4], [1, 2]]))), PHP_EOL;
  echo rtrim(llStr(cartN([[1, 2], []]))), PHP_EOL;
  echo rtrim(llStr(cartN([[], [1, 2]]))), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  echo rtrim('['), PHP_EOL;
  foreach (cartN([[1776, 1789], [7, 12], [4, 14, 23], [0, 1]]) as $p) {
  echo rtrim(' ' . listStr($p)), PHP_EOL;
};
  echo rtrim(']'), PHP_EOL;
  echo rtrim(llStr(cartN([[1, 2, 3], [30], [500, 100]]))), PHP_EOL;
  echo rtrim(llStr(cartN([[1, 2, 3], [], [500, 100]]))), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  echo rtrim(llStr(cartN(null))), PHP_EOL;
  echo rtrim(llStr(cartN([]))), PHP_EOL;
}
main();
