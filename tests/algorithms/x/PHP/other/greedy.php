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
  function get_value($t) {
  global $food, $value, $weight, $foods, $res;
  return $t['value'];
};
  function get_weight($t) {
  global $food, $value, $weight, $foods, $res;
  return $t['weight'];
};
  function get_name($t) {
  global $food, $value, $weight, $foods, $res;
  return $t['name'];
};
  function value_weight($t) {
  global $food, $value, $weight, $foods, $res;
  return $t['value'] / $t['weight'];
};
  function build_menu($names, $values, $weights) {
  global $food, $value, $weight, $foods, $res;
  $menu = [];
  $i = 0;
  while ($i < count($values) && $i < count($names) && $i < count($weights)) {
  $menu = _append($menu, ['name' => $names[$i], $value => $values[$i], $weight => $weights[$i]]);
  $i = $i + 1;
};
  return $menu;
};
  function sort_desc($items, $key_func) {
  global $food, $value, $weight, $foods, $res;
  $arr = [];
  $i = 0;
  while ($i < count($items)) {
  $arr = _append($arr, $items[$i]);
  $i = $i + 1;
};
  $j = 1;
  while ($j < count($arr)) {
  $key_item = $arr[$j];
  $key_val = $key_func($key_item);
  $k = $j - 1;
  while ($k >= 0 && $key_func($arr[$k]) < $key_val) {
  $arr[$k + 1] = $arr[$k];
  $k = $k - 1;
};
  $arr[$k + 1] = $key_item;
  $j = $j + 1;
};
  return $arr;
};
  function greedy($items, $max_cost, $key_func) {
  global $food, $value, $weight, $foods, $res;
  $items_copy = sort_desc($items, $key_func);
  $result = [];
  $total_value = 0.0;
  $total_cost = 0.0;
  $i = 0;
  while ($i < count($items_copy)) {
  $it = $items_copy[$i];
  $w = get_weight($it);
  if ($total_cost + $w <= $max_cost) {
  $result = _append($result, $it);
  $total_cost = $total_cost + $w;
  $total_value = $total_value + get_value($it);
}
  $i = $i + 1;
};
  return ['items' => $result, 'total_value' => $total_value];
};
  function thing_to_string($t) {
  global $food, $value, $weight, $foods, $res;
  return 'Thing(' . $t['name'] . ', ' . _str($t['value']) . ', ' . _str($t['weight']) . ')';
};
  function list_to_string($ts) {
  global $food, $value, $weight, $foods, $res;
  $s = '[';
  $i = 0;
  while ($i < count($ts)) {
  $s = $s . thing_to_string($ts[$i]);
  if ($i < count($ts) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  $food = ['Burger', 'Pizza', 'Coca Cola', 'Rice', 'Sambhar', 'Chicken', 'Fries', 'Milk'];
  $value = [80.0, 100.0, 60.0, 70.0, 50.0, 110.0, 90.0, 60.0];
  $weight = [40.0, 60.0, 40.0, 70.0, 100.0, 85.0, 55.0, 70.0];
  $foods = build_menu($food, $value, $weight);
  echo rtrim(list_to_string($foods)), PHP_EOL;
  $res = greedy($foods, 500.0, 'get_value');
  echo rtrim(list_to_string($res['items'])), PHP_EOL;
  echo rtrim(_str($res['total_value'])), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
