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
  function mochi_floor($x) {
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
};
  function pow10($n) {
  $result = 1.0;
  $i = 0;
  while ($i < $n) {
  $result = $result * 10.0;
  $i = $i + 1;
};
  return $result;
};
  function mochi_round($x, $n) {
  $m = pow10($n);
  $y = floatval(mochi_floor($x * $m + 0.5));
  return $y / $m;
};
  function sqrtApprox($x) {
  $guess = $x;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function mean($data) {
  $total = 0.0;
  $i = 0;
  $n = count($data);
  while ($i < $n) {
  $total = $total + $data[$i];
  $i = $i + 1;
};
  return $total / (floatval($n));
};
  function stdev($data) {
  $n = count($data);
  if ($n <= 1) {
  $panic('data length must be > 1');
}
  $m = mean($data);
  $sum_sq = 0.0;
  $i = 0;
  while ($i < $n) {
  $diff = $data[$i] - $m;
  $sum_sq = $sum_sq + $diff * $diff;
  $i = $i + 1;
};
  return sqrtApprox($sum_sq / (floatval(($n - 1))));
};
  function normalization($data, $ndigits) {
  $x_min = floatval(min($data));
  $x_max = floatval(max($data));
  $denom = $x_max - $x_min;
  $result = [];
  $i = 0;
  $n = count($data);
  while ($i < $n) {
  $norm = ($data[$i] - $x_min) / $denom;
  $result = _append($result, mochi_round($norm, $ndigits));
  $i = $i + 1;
};
  return $result;
};
  function standardization($data, $ndigits) {
  $mu = mean($data);
  $sigma = stdev($data);
  $result = [];
  $i = 0;
  $n = count($data);
  while ($i < $n) {
  $z = ($data[$i] - $mu) / $sigma;
  $result = _append($result, mochi_round($z, $ndigits));
  $i = $i + 1;
};
  return $result;
};
  echo rtrim(_str(normalization([2.0, 7.0, 10.0, 20.0, 30.0, 50.0], 3))), PHP_EOL;
  echo rtrim(_str(normalization([5.0, 10.0, 15.0, 20.0, 25.0], 3))), PHP_EOL;
  echo rtrim(_str(standardization([2.0, 7.0, 10.0, 20.0, 30.0, 50.0], 3))), PHP_EOL;
  echo rtrim(_str(standardization([5.0, 10.0, 15.0, 20.0, 25.0], 3))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
