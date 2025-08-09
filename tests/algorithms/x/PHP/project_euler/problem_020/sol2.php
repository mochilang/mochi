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
function factorial($n) {
  $result = 1;
  $i = 2;
  while ($i <= $n) {
  $result = $result * $i;
  $i = $i + 1;
};
  return $result;
}
function digit_sum($n) {
  $total = 0;
  $m = $n;
  while ($m > 0) {
  $total = $total + ($m % 10);
  $m = _intdiv($m, 10);
};
  return $total;
}
function solution($num) {
  $f = factorial($num);
  return digit_sum($f);
}
function main() {
  echo rtrim(json_encode(solution(100), 1344)), PHP_EOL;
  echo rtrim(json_encode(solution(50), 1344)), PHP_EOL;
  echo rtrim(json_encode(solution(10), 1344)), PHP_EOL;
  echo rtrim(json_encode(solution(5), 1344)), PHP_EOL;
  echo rtrim(json_encode(solution(3), 1344)), PHP_EOL;
  echo rtrim(json_encode(solution(2), 1344)), PHP_EOL;
  echo rtrim(json_encode(solution(1), 1344)), PHP_EOL;
}
main();
