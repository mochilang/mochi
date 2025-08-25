<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        $q = bcdiv($sa, $sb, 0);
        $rem = bcmod($sa, $sb);
        $neg = ((strpos($sa, '-') === 0) xor (strpos($sb, '-') === 0));
        if ($neg && bccomp($rem, '0') != 0) {
            $q = bcsub($q, '1');
        }
        return intval($q);
    }
    $ai = intval($a);
    $bi = intval($b);
    $q = intdiv($ai, $bi);
    if ((($ai ^ $bi) < 0) && ($ai % $bi != 0)) {
        $q -= 1;
    }
    return $q;
}
function perfect($n) {
  if ($n <= 0) {
  return false;
}
  $limit = _intdiv($n, 2);
  $sum = 0;
  $i = 1;
  while ($i <= $limit) {
  if ($n % $i == 0) {
  $sum = $sum + $i;
}
  $i = $i + 1;
};
  return $sum == $n;
}
function main() {
  $numbers = [6, 28, 29, 12, 496, 8128, 0, -1];
  $idx = 0;
  while ($idx < count($numbers)) {
  $num = $numbers[$idx];
  if (perfect($num)) {
  echo rtrim(_str($num) . ' is a Perfect Number.'), PHP_EOL;
} else {
  echo rtrim(_str($num) . ' is not a Perfect Number.'), PHP_EOL;
}
  $idx = $idx + 1;
};
}
main();
