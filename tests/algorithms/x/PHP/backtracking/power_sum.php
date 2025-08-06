<?php
ini_set('memory_limit', '-1');
function int_pow($base, $exp) {
  $result = 1;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
}
function backtrack($target, $exp, $current, $current_sum) {
  if ($current_sum == $target) {
  return 1;
}
  $p = int_pow($current, $exp);
  $count = 0;
  if ($current_sum + $p <= $target) {
  $count = $count + backtrack($target, $exp, $current + 1, $current_sum + $p);
}
  if ($p < $target) {
  $count = $count + backtrack($target, $exp, $current + 1, $current_sum);
}
  return $count;
}
function solve($target, $exp) {
  if (!(1 <= $target && $target <= 1000 && 2 <= $exp && $exp <= 10)) {
  echo rtrim('Invalid input'), PHP_EOL;
  return 0;
}
  return backtrack($target, $exp, 1, 0);
}
echo rtrim(json_encode(solve(13, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(solve(10, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(solve(10, 3), 1344)), PHP_EOL;
