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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function get_avg($number_1, $number_2) {
  return _intdiv(($number_1 + $number_2), 2);
};
  function guess_the_number($lower, $higher, $to_guess) {
  if ($lower > $higher) {
  _panic('argument value for lower and higher must be(lower > higher)');
}
  if (!($lower < $to_guess && $to_guess < $higher)) {
  _panic('guess value must be within the range of lower and higher value');
}
  $answer = null;
$answer = function($number) use (&$answer, $lower, $higher, $to_guess) {
  if ($number > $to_guess) {
  return 'high';
} else {
  if ($number < $to_guess) {
  return 'low';
} else {
  return 'same';
};
}
};
  echo rtrim('started...'), PHP_EOL;
  $last_lowest = $lower;
  $last_highest = $higher;
  $last_numbers = [];
  while (true) {
  $number = get_avg($last_lowest, $last_highest);
  $last_numbers = _append($last_numbers, $number);
  $resp = $answer($number);
  if ($resp == 'low') {
  $last_lowest = $number;
} else {
  if ($resp == 'high') {
  $last_highest = $number;
} else {
  break;
};
}
};
  echo rtrim('guess the number : ' . _str($last_numbers[count($last_numbers) - 1])), PHP_EOL;
  echo rtrim('details : ' . _str($last_numbers)), PHP_EOL;
  return $last_numbers;
};
  guess_the_number(10, 1000, 17);
  guess_the_number(-10000, 10000, 7);
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
