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
function get_gas_stations($gas_quantities, $costs) {
  global $example1, $example2;
  $stations = [];
  $i = 0;
  while ($i < count($gas_quantities)) {
  $stations = _append($stations, ['gas_quantity' => $gas_quantities[$i], 'cost' => $costs[$i]]);
  $i = $i + 1;
};
  return $stations;
}
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
}
$example1 = get_gas_stations([1, 2, 3, 4, 5], [3, 4, 5, 1, 2]);
echo rtrim(_str(can_complete_journey($example1))), PHP_EOL;
$example2 = get_gas_stations([2, 3, 4], [3, 4, 3]);
echo rtrim(_str(can_complete_journey($example2))), PHP_EOL;
