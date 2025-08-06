<?php
ini_set('memory_limit', '-1');
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
function format_ruleset($ruleset) {
  global $initial, $cells, $rules, $time, $next, $t;
  $rs = $ruleset;
  $bits_rev = [];
  $i = 0;
  while ($i < 8) {
  $bits_rev = _append($bits_rev, $rs % 2);
  $rs = _intdiv($rs, 2);
  $i = $i + 1;
};
  $bits = [];
  $j = count($bits_rev) - 1;
  while ($j >= 0) {
  $bits = _append($bits, $bits_rev[$j]);
  $j = $j - 1;
};
  return $bits;
}
function new_generation($cells, $rules, $time) {
  global $initial, $next, $t;
  $population = count($cells[0]);
  $next_generation = [];
  $i = 0;
  while ($i < $population) {
  $left_neighbor = ($i == 0 ? 0 : $cells[$time][$i - 1]);
  $right_neighbor = ($i == $population - 1 ? 0 : $cells[$time][$i + 1]);
  $center = $cells[$time][$i];
  $idx = 7 - ($left_neighbor * 4 + $center * 2 + $right_neighbor);
  $next_generation = _append($next_generation, $rules[$idx]);
  $i = $i + 1;
};
  return $next_generation;
}
function cells_to_string($row) {
  global $initial, $cells, $rules, $time, $next, $t;
  $result = '';
  $i = 0;
  while ($i < count($row)) {
  if ($row[$i] == 1) {
  $result = $result . '#';
} else {
  $result = $result . '.';
}
  $i = $i + 1;
};
  return $result;
}
$initial = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
$cells = [$initial];
$rules = format_ruleset(30);
$time = 0;
while ($time < 16) {
  $next = new_generation($cells, $rules, $time);
  $cells = _append($cells, $next);
  $time = $time + 1;
}
$t = 0;
while ($t < count($cells)) {
  echo rtrim(cells_to_string($cells[$t])), PHP_EOL;
  $t = $t + 1;
}
