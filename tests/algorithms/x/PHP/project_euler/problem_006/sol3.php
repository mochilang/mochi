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
function solution($n) {
  $i = 1;
  $sum = 0;
  $sum_of_squares = 0;
  while ($i <= $n) {
  $sum = $sum + $i;
  $sum_of_squares = $sum_of_squares + $i * $i;
  $i = $i + 1;
};
  $square_of_sum = $sum * $sum;
  return $square_of_sum - $sum_of_squares;
}
echo rtrim('solution() = ' . _str(solution(100))), PHP_EOL;
