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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
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
  global $input_str;
  $counters = null;
  $i = 0;
  while ($i <= $n) {
  $counters = _append($counters, 0);
  $i = $i + 1;
};
  $counters[1] = 1;
  $largest_number = 1;
  $pre_counter = 1;
  $start = 2;
  while ($start < $n) {
  $number = $start;
  $counter = 0;
  while (true) {
  if ($number < count($counters) && $counters[$number] != 0) {
  $counter = $counter + $counters[$number];
  break;
}
  if ($number % 2 == 0) {
  $number = _intdiv($number, 2);
} else {
  $number = 3 * $number + 1;
}
  $counter = $counter + 1;
};
  if ($start < count($counters) && $counters[$start] == 0) {
  $counters[$start] = $counter;
}
  if ($counter > $pre_counter) {
  $largest_number = $start;
  $pre_counter = $counter;
}
  $start = $start + 1;
};
  return $largest_number;
}
$input_str = trim(fgets(STDIN));
$n = intval($input_str);
echo rtrim(_str(solution($n))), PHP_EOL;
