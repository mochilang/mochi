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
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function peak($lst) {
  $low = 0;
  $high = count($lst) - 1;
  while ($low < $high) {
  $mid = _intdiv(($low + $high), 2);
  if ($lst[$mid] < $lst[$mid + 1]) {
  $low = $mid + 1;
} else {
  $high = $mid;
}
};
  return $lst[$low];
}
function main() {
  echo rtrim(_str(peak([1, 2, 3, 4, 5, 4, 3, 2, 1]))), PHP_EOL;
  echo rtrim(_str(peak([1, 10, 9, 8, 7, 6, 5, 4]))), PHP_EOL;
  echo rtrim(_str(peak([1, 9, 8, 7]))), PHP_EOL;
  echo rtrim(_str(peak([1, 2, 3, 4, 5, 6, 7, 0]))), PHP_EOL;
  echo rtrim(_str(peak([1, 2, 3, 4, 3, 2, 1, 0, -1, -2]))), PHP_EOL;
}
main();
