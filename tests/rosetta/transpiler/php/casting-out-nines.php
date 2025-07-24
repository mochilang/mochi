<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
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
function parseIntBase($s, $base) {
  global $intToBase, $subset, $testCases, $idx, $tc, $sx, $valid, $k, $found;
  $digits = '0123456789abcdefghijklmnopqrstuvwxyz';
  $n = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $j = 0;
  $v = 0;
  while ($j < strlen($digits)) {
  if (substr($digits, $j, $j + 1 - $j) == substr($s, $i, $i + 1 - $i)) {
  $v = $j;
  break;
}
  $j = $j + 1;
};
  $n = $n * $base + $v;
  $i = $i + 1;
};
  return $n;
}
function intToBase($n, $base) {
  global $parseIntBase, $subset, $testCases, $idx, $tc, $s, $sx, $valid, $i, $k, $found;
  $digits = '0123456789abcdefghijklmnopqrstuvwxyz';
  if ($n == 0) {
  return '0';
}
  $out = '';
  $v = $n;
  while ($v > 0) {
  $d = $v % $base;
  $out = substr($digits, $d, $d + 1 - $d) . $out;
  $v = intdiv($v, $base);
};
  return $out;
}
function subset($base, $begin, $end) {
  global $parseIntBase, $intToBase, $testCases, $idx, $tc, $s, $sx, $valid, $i, $found;
  $b = parseIntBase($begin, $base);
  $e = parseIntBase($end, $base);
  $out = [];
  $k = $b;
  while ($k <= $e) {
  $ks = intToBase($k, $base);
  $mod = $base - 1;
  $r1 = parseIntBase($ks, $base) % $mod;
  $r2 = (parseIntBase($ks, $base) * parseIntBase($ks, $base)) % $mod;
  if ($r1 == $r2) {
  $out = array_merge($out, [$ks]);
}
  $k = $k + 1;
};
  return $out;
}
$testCases = [['base' => 10, 'begin' => '1', 'end' => '100', 'kaprekar' => ['1', '9', '45', '55', '99']], ['base' => 17, 'begin' => '10', 'end' => 'gg', 'kaprekar' => ['3d', 'd4', 'gg']]];
$idx = 0;
while ($idx < count($testCases)) {
  $tc = $testCases[$idx];
  echo rtrim('
Test case base = ' . _str($tc['base']) . ', begin = ' . $tc['begin'] . ', end = ' . $tc['end'] . ':'), PHP_EOL;
  $s = subset($tc['base'], $tc['begin'], $tc['end']);
  echo rtrim('Subset:  ' . _str($s)), PHP_EOL;
  echo rtrim('Kaprekar:' . _str($tc['kaprekar'])), PHP_EOL;
  $sx = 0;
  $valid = true;
  $i = 0;
  while ($i < _len($tc['kaprekar'])) {
  $k = $tc['kaprekar'][$i];
  $found = false;
  while ($sx < count($s)) {
  if ($s[$sx] == $k) {
  $found = true;
  $sx = $sx + 1;
  break;
}
  $sx = $sx + 1;
};
  if (!$found) {
  echo rtrim('Fail:' . $k . ' not in subset'), PHP_EOL;
  $valid = false;
  break;
}
  $i = $i + 1;
};
  if ($valid) {
  echo rtrim('Valid subset.'), PHP_EOL;
}
  $idx = $idx + 1;
}
