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
  function get_gas_stations($gas_quantities, $costs) {
  global $example1, $example2;
  $stations = [];
  $i = 0;
  while ($i < count($gas_quantities)) {
  $stations = _append($stations, ['gas_quantity' => $gas_quantities[$i], 'cost' => $costs[$i]]);
  $i = $i + 1;
};
  return $stations;
};
  function can_complete_journey($gas_stations) {
  global $example1, $example2;
  $total_gas = 0;
  $total_cost = 0;
  $i = 0;
  while ($i < count($gas_stations)) {
  $total_gas = $total_gas + $gas_stations[$i]['gas_quantity'];
  $total_cost = $total_cost + $gas_stations[$i]['cost'];
  $i = $i + 1;
};
  if ($total_gas < $total_cost) {
  return -1;
}
  $start = 0;
  $net = 0;
  $i = 0;
  while ($i < count($gas_stations)) {
  $station = $gas_stations[$i];
  $net = $net + $station['gas_quantity'] - $station['cost'];
  if ($net < 0) {
  $start = $i + 1;
  $net = 0;
}
  $i = $i + 1;
};
  return $start;
};
  $example1 = get_gas_stations([1, 2, 3, 4, 5], [3, 4, 5, 1, 2]);
  echo rtrim(_str(can_complete_journey($example1))), PHP_EOL;
  $example2 = get_gas_stations([2, 3, 4], [3, 4, 3]);
  echo rtrim(_str(can_complete_journey($example2))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
