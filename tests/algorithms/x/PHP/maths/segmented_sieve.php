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
$__start_mem = memory_get_usage();
$__start = _now();
  function min_int($a, $b) {
  if ($a < $b) {
  return $a;
}
  return $b;
};
  function int_sqrt($n) {
  $r = 0;
  while (($r + 1) * ($r + 1) <= $n) {
  $r = $r + 1;
};
  return $r;
};
  function sieve($n) {
  if ($n <= 0) {
  $panic('Number must instead be a positive integer');
}
  $in_prime = [];
  $start = 2;
  $end = int_sqrt($n);
  $temp = [];
  $i = 0;
  while ($i < $end + 1) {
  $temp = _append($temp, 1);
  $i = $i + 1;
};
  $prime = [];
  while ($start <= $end) {
  if ($temp[$start] == 1) {
  $in_prime = _append($in_prime, $start);
  $j = $start * $start;
  while ($j <= $end) {
  $temp[$j] = 0;
  $j = $j + $start;
};
}
  $start = $start + 1;
};
  $i = 0;
  while ($i < count($in_prime)) {
  $prime = _append($prime, $in_prime[$i]);
  $i = $i + 1;
};
  $low = $end + 1;
  $high = min_int(2 * $end, $n);
  while ($low <= $n) {
  $tempSeg = [];
  $size = $high - $low + 1;
  $k = 0;
  while ($k < $size) {
  $tempSeg = _append($tempSeg, 1);
  $k = $k + 1;
};
  $idx = 0;
  while ($idx < count($in_prime)) {
  $each = $in_prime[$idx];
  $t = (_intdiv($low, $each)) * $each;
  if ($t < $low) {
  $t = $t + $each;
}
  $j2 = $t;
  while ($j2 <= $high) {
  $tempSeg[$j2 - $low] = 0;
  $j2 = $j2 + $each;
};
  $idx = $idx + 1;
};
  $j3 = 0;
  while ($j3 < count($tempSeg)) {
  if ($tempSeg[$j3] == 1) {
  $prime = _append($prime, $j3 + $low);
}
  $j3 = $j3 + 1;
};
  $low = $high + 1;
  $high = min_int($high + $end, $n);
};
  return $prime;
};
  function lists_equal($a, $b) {
  if (count($a) != count($b)) {
  return false;
}
  $m = 0;
  while ($m < count($a)) {
  if ($a[$m] != $b[$m]) {
  return false;
}
  $m = $m + 1;
};
  return true;
};
  function test_sieve() {
  $e1 = sieve(8);
  if (!lists_equal($e1, [2, 3, 5, 7])) {
  $panic('sieve(8) failed');
}
  $e2 = sieve(27);
  if (!lists_equal($e2, [2, 3, 5, 7, 11, 13, 17, 19, 23])) {
  $panic('sieve(27) failed');
}
};
  function main() {
  test_sieve();
  echo rtrim(_str(sieve(30))), PHP_EOL;
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
