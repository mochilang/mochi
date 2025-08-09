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
function solution($n) {
  $ones_counts = [0, 3, 3, 5, 4, 4, 3, 5, 5, 4, 3, 6, 6, 8, 8, 7, 7, 9, 8, 8];
  $tens_counts = [0, 0, 6, 6, 5, 5, 5, 7, 6, 6];
  $count = 0;
  $i = 1;
  while ($i <= $n) {
  if ($i < 1000) {
  if ($i >= 100) {
  $count = $count + $ones_counts[_intdiv($i, 100)] + 7;
  if ($i % 100 != 0) {
  $count = $count + 3;
};
};
  $remainder = $i % 100;
  if ($remainder > 0 && $remainder < 20) {
  $count = $count + $ones_counts[$remainder];
} else {
  $count = $count + $ones_counts[$i % 10];
  $count = $count + $tens_counts[_intdiv(($remainder - $i % 10), 10)];
};
} else {
  $count = $count + $ones_counts[_intdiv($i, 1000)] + 8;
}
  $i = $i + 1;
};
  return $count;
}
echo rtrim(_str(solution(1000))), PHP_EOL;
echo rtrim(_str(solution(5))), PHP_EOL;
