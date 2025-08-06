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
function maximum_non_adjacent_sum($nums) {
  if (count($nums) == 0) {
  return 0;
}
  $max_including = $nums[0];
  $max_excluding = 0;
  $i = 1;
  while ($i < count($nums)) {
  $num = $nums[$i];
  $new_including = $max_excluding + $num;
  $new_excluding = ($max_including > $max_excluding ? $max_including : $max_excluding);
  $max_including = $new_including;
  $max_excluding = $new_excluding;
  $i = $i + 1;
};
  if ($max_including > $max_excluding) {
  return $max_including;
}
  return $max_excluding;
}
echo rtrim(_str(maximum_non_adjacent_sum([1, 2, 3]))), PHP_EOL;
echo rtrim(_str(maximum_non_adjacent_sum([1, 5, 3, 7, 2, 2, 6]))), PHP_EOL;
echo rtrim(_str(maximum_non_adjacent_sum([-1, -5, -3, -7, -2, -2, -6]))), PHP_EOL;
echo rtrim(_str(maximum_non_adjacent_sum([499, 500, -3, -7, -2, -2, -6]))), PHP_EOL;
