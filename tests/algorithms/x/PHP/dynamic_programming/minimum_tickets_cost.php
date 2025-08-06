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
function make_list($len, $value) {
  $arr = [];
  $i = 0;
  while ($i < $len) {
  $arr = _append($arr, $value);
  $i = $i + 1;
};
  return $arr;
}
function max_int($a, $b) {
  if ($a > $b) {
  return $a;
} else {
  return $b;
}
}
function min_int($a, $b) {
  if ($a < $b) {
  return $a;
} else {
  return $b;
}
}
function min3($a, $b, $c) {
  return min_int(min_int($a, $b), $c);
}
function minimum_tickets_cost($days, $costs) {
  if (count($days) == 0) {
  return 0;
}
  $last_day = $days[count($days) - 1];
  $dp = make_list($last_day + 1, 0);
  $day_index = 0;
  $d = 1;
  while ($d <= $last_day) {
  if ($day_index < count($days) && $d == $days[$day_index]) {
  $cost1 = $dp[$d - 1] + $costs[0];
  $cost7 = $dp[max_int(0, $d - 7)] + $costs[1];
  $cost30 = $dp[max_int(0, $d - 30)] + $costs[2];
  $dp[$d] = min3($cost1, $cost7, $cost30);
  $day_index = $day_index + 1;
} else {
  $dp[$d] = $dp[$d - 1];
}
  $d = $d + 1;
};
  return $dp[$last_day];
}
echo rtrim(_str(minimum_tickets_cost([1, 4, 6, 7, 8, 20], [2, 7, 15]))), PHP_EOL;
echo rtrim(_str(minimum_tickets_cost([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30, 31], [2, 7, 15]))), PHP_EOL;
echo rtrim(_str(minimum_tickets_cost([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30, 31], [2, 90, 150]))), PHP_EOL;
