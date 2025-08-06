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
function minimum_subarray_sum($target, $numbers) {
  $n = count($numbers);
  if ($n == 0) {
  return 0;
}
  if ($target == 0) {
  $i = 0;
  while ($i < $n) {
  if ($numbers[$i] == 0) {
  return 0;
}
  $i = $i + 1;
};
}
  $left = 0;
  $right = 0;
  $curr_sum = 0;
  $min_len = $n + 1;
  while ($right < $n) {
  $curr_sum = $curr_sum + $numbers[$right];
  while ($curr_sum >= $target && $left <= $right) {
  $current_len = $right - $left + 1;
  if ($current_len < $min_len) {
  $min_len = $current_len;
}
  $curr_sum = $curr_sum - $numbers[$left];
  $left = $left + 1;
};
  $right = $right + 1;
};
  if ($min_len == $n + 1) {
  return 0;
}
  return $min_len;
}
echo rtrim(_str(minimum_subarray_sum(7, [2, 3, 1, 2, 4, 3]))), PHP_EOL;
echo rtrim(_str(minimum_subarray_sum(7, [2, 3, -1, 2, 4, -3]))), PHP_EOL;
echo rtrim(_str(minimum_subarray_sum(11, [1, 1, 1, 1, 1, 1, 1, 1]))), PHP_EOL;
echo rtrim(_str(minimum_subarray_sum(0, [1, 2, 3]))), PHP_EOL;
