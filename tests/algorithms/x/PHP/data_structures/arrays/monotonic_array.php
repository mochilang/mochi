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
function is_monotonic($nums) {
  if (count($nums) <= 2) {
  return true;
}
  $increasing = true;
  $decreasing = true;
  $i = 0;
  while ($i < count($nums) - 1) {
  if ($nums[$i] > $nums[$i + 1]) {
  $increasing = false;
}
  if ($nums[$i] < $nums[$i + 1]) {
  $decreasing = false;
}
  $i = $i + 1;
};
  return $increasing || $decreasing;
}
echo rtrim(_str(is_monotonic([1, 2, 2, 3]))), PHP_EOL;
echo rtrim(_str(is_monotonic([6, 5, 4, 4]))), PHP_EOL;
echo rtrim(_str(is_monotonic([1, 3, 2]))), PHP_EOL;
