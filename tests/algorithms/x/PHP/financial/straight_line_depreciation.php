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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function straight_line_depreciation($useful_years, $purchase_value, $residual_value) {
  if ($useful_years < 1) {
  _panic('Useful years cannot be less than 1');
}
  if ($purchase_value < 0.0) {
  _panic('Purchase value cannot be less than zero');
}
  if ($purchase_value < $residual_value) {
  _panic('Purchase value cannot be less than residual value');
}
  $depreciable_cost = $purchase_value - $residual_value;
  $annual_expense = $depreciable_cost / (1.0 * $useful_years);
  $expenses = [];
  $accumulated = 0.0;
  $period = 0;
  while ($period < $useful_years) {
  if ($period != $useful_years - 1) {
  $accumulated = $accumulated + $annual_expense;
  $expenses = _append($expenses, $annual_expense);
} else {
  $end_year_expense = $depreciable_cost - $accumulated;
  $expenses = _append($expenses, $end_year_expense);
}
  $period = $period + 1;
};
  return $expenses;
};
  echo rtrim(_str(straight_line_depreciation(10, 1100.0, 100.0))), PHP_EOL;
  echo rtrim(_str(straight_line_depreciation(6, 1250.0, 50.0))), PHP_EOL;
  echo rtrim(_str(straight_line_depreciation(4, 1001.0, 0.0))), PHP_EOL;
  echo rtrim(_str(straight_line_depreciation(11, 380.0, 50.0))), PHP_EOL;
  echo rtrim(_str(straight_line_depreciation(1, 4985.0, 100.0))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
