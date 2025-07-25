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
function padLeft($n, $width) {
  $s = _str($n);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
}
function modPow($base, $exp, $mod) {
  $result = 1 % $mod;
  $b = $base % $mod;
  $e = $exp;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $result = ($result * $b) % $mod;
}
  $b = ($b * $b) % $mod;
  $e = _intdiv($e, 2);
};
  return $result;
}
function main() {
  $k = 2;
  while ($k <= 10) {
  echo rtrim('The first 50 Curzon numbers using a base of ' . _str($k) . ' :'), PHP_EOL;
  $count = 0;
  $n = 1;
  $curzon50 = [];
  while (true) {
  $d = $k * $n + 1;
  if ((modPow($k, $n, $d) + 1) % $d == 0) {
  if ($count < 50) {
  $curzon50 = array_merge($curzon50, [$n]);
};
  $count = $count + 1;
  if ($count == 50) {
  $idx = 0;
  while ($idx < count($curzon50)) {
  $line = '';
  $j = 0;
  while ($j < 10) {
  $line = $line . padLeft($curzon50[$idx], 4) . ' ';
  $idx = $idx + 1;
  $j = $j + 1;
};
  echo rtrim(json_encode(substr($line, 0, strlen($line) - 1 - 0), 1344)), PHP_EOL;
};
};
  if ($count == 1000) {
  echo rtrim('
One thousandth: ' . _str($n)), PHP_EOL;
  break;
};
}
  $n = $n + 1;
};
  echo rtrim(''), PHP_EOL;
  $k = $k + 2;
};
}
main();
