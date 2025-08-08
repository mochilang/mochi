<?php
ini_set('memory_limit', '-1');
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function solution($n) {
  $sum_of_squares = _intdiv($n * ($n + 1) * (2 * $n + 1), 6);
  $sum_first_n = _intdiv($n * ($n + 1), 2);
  $square_of_sum = $sum_first_n * $sum_first_n;
  return $square_of_sum - $sum_of_squares;
}
echo rtrim(json_encode(solution(10), 1344)), PHP_EOL;
echo rtrim(json_encode(solution(15), 1344)), PHP_EOL;
echo rtrim(json_encode(solution(20), 1344)), PHP_EOL;
echo rtrim(json_encode(solution(50), 1344)), PHP_EOL;
echo rtrim(json_encode(solution(100), 1344)), PHP_EOL;
