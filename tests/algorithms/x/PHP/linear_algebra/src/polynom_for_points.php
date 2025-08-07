<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
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
$__start_mem = memory_get_usage();
$__start = _now();
  function contains_int($xs, $x) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function split($s, $sep) {
  $res = [];
  $current = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == $sep) {
  $res = _append($res, $current);
  $current = '';
} else {
  $current = $current . $ch;
}
  $i = $i + 1;
};
  $res = _append($res, $current);
  return $res;
};
  function pow_int_float($base, $exp) {
  $result = 1.0;
  $i = 0;
  while ($i < $exp) {
  $result = $result * (floatval($base));
  $i = $i + 1;
};
  return $result;
};
  function points_to_polynomial($coordinates) {
  if (count($coordinates) == 0) {
  $panic('The program cannot work out a fitting polynomial.');
}
  $i = 0;
  while ($i < count($coordinates)) {
  if (count($coordinates[$i]) != 2) {
  $panic('The program cannot work out a fitting polynomial.');
}
  $i = $i + 1;
};
  $j = 0;
  while ($j < count($coordinates)) {
  $k = $j + 1;
  while ($k < count($coordinates)) {
  if ($coordinates[$j][0] == $coordinates[$k][0] && $coordinates[$j][1] == $coordinates[$k][1]) {
  $panic('The program cannot work out a fitting polynomial.');
}
  $k = $k + 1;
};
  $j = $j + 1;
};
  $set_x = [];
  $i = 0;
  while ($i < count($coordinates)) {
  $x_val = $coordinates[$i][0];
  if (!contains_int($set_x, $x_val)) {
  $set_x = _append($set_x, $x_val);
}
  $i = $i + 1;
};
  if (count($set_x) == 1) {
  return 'x=' . _str($coordinates[0][0]);
}
  if (count($set_x) != count($coordinates)) {
  $panic('The program cannot work out a fitting polynomial.');
}
  $n = count($coordinates);
  $matrix = [];
  $row = 0;
  while ($row < $n) {
  $line = [];
  $col = 0;
  while ($col < $n) {
  $power = pow_int_float($coordinates[$row][0], $n - ($col + 1));
  $line = _append($line, $power);
  $col = $col + 1;
};
  $matrix = _append($matrix, $line);
  $row = $row + 1;
};
  $vector = [];
  $row = 0;
  while ($row < $n) {
  $vector = _append($vector, floatval($coordinates[$row][1]));
  $row = $row + 1;
};
  $count = 0;
  while ($count < $n) {
  $number = 0;
  while ($number < $n) {
  if ($count != $number) {
  $fraction = $matrix[$number][$count] / $matrix[$count][$count];
  $cc = 0;
  while ($cc < $n) {
  $matrix[$number][$cc] = $matrix[$number][$cc] - $matrix[$count][$cc] * $fraction;
  $cc = $cc + 1;
};
  $vector[$number] = $vector[$number] - $vector[$count] * $fraction;
}
  $number = $number + 1;
};
  $count = $count + 1;
};
  $solution = [];
  $count = 0;
  while ($count < $n) {
  $value = $vector[$count] / $matrix[$count][$count];
  $solution = _append($solution, _str($value));
  $count = $count + 1;
};
  $solved = 'f(x)=';
  $count = 0;
  while ($count < $n) {
  $parts = explode('e', $solution[$count]);
  $coeff = $solution[$count];
  if (count($parts) > 1) {
  $coeff = $parts[0] . '*10^' . $parts[1];
}
  $solved = $solved . 'x^' . _str($n - ($count + 1)) . '*' . $coeff;
  if ($count + 1 != $n) {
  $solved = $solved . '+';
}
  $count = $count + 1;
};
  return $solved;
};
  function main() {
  echo rtrim(points_to_polynomial([[1, 0], [2, 0], [3, 0]])), PHP_EOL;
  echo rtrim(points_to_polynomial([[1, 1], [2, 1], [3, 1]])), PHP_EOL;
  echo rtrim(points_to_polynomial([[1, 1], [2, 4], [3, 9]])), PHP_EOL;
  echo rtrim(points_to_polynomial([[1, 3], [2, 6], [3, 11]])), PHP_EOL;
  echo rtrim(points_to_polynomial([[1, -3], [2, -6], [3, -11]])), PHP_EOL;
  echo rtrim(points_to_polynomial([[1, 1], [1, 2], [1, 3]])), PHP_EOL;
  echo rtrim(points_to_polynomial([[1, 5], [2, 2], [3, 9]])), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
