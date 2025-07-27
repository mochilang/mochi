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
function mochi_pow($base, $exp) {
  $result = 1;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
}
function isDisarium($n) {
  $digits = [];
  $x = $n;
  if ($x == 0) {
  $digits = array_merge($digits, [0]);
}
  while ($x > 0) {
  $digits = array_merge($digits, [$x % 10]);
  $x = intval((_intdiv($x, 10)));
};
  $sum = 0;
  $pos = 1;
  $i = count($digits) - 1;
  while ($i >= 0) {
  $sum = $sum + mochi_pow($digits[$i], $pos);
  $pos = $pos + 1;
  $i = $i - 1;
};
  return $sum == $n;
}
function main() {
  $count = 0;
  $n = 0;
  while ($count < 19 && $n < 3000000) {
  if (isDisarium($n)) {
  echo rtrim(_str($n)), PHP_EOL;
  $count = $count + 1;
}
  $n = $n + 1;
};
  echo rtrim('
Found the first ' . _str($count) . ' Disarium numbers.'), PHP_EOL;
}
main();
