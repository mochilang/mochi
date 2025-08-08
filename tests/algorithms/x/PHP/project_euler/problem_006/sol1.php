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
  $sum_of_squares = 0;
  $sum_of_ints = 0;
  $i = 1;
  while ($i <= $n) {
  $sum_of_squares = $sum_of_squares + $i * $i;
  $sum_of_ints = $sum_of_ints + $i;
  $i = $i + 1;
};
  return $sum_of_ints * $sum_of_ints - $sum_of_squares;
}
function main() {
  echo rtrim(_str(solution(100))), PHP_EOL;
}
main();
