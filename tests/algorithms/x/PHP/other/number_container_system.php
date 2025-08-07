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
  function remove_at($xs, $idx) {
  global $nm, $im, $cont;
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  if ($i != $idx) {
  $res = _append($res, $xs[$i]);
}
  $i = $i + 1;
};
  return $res;
};
  function insert_at($xs, $idx, $val) {
  global $nm, $im, $cont;
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  if ($i == $idx) {
  $res = _append($res, $val);
}
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  if ($idx == count($xs)) {
  $res = _append($res, $val);
}
  return $res;
};
  function binary_search_delete($array, $item) {
  global $nm, $im, $cont;
  $low = 0;
  $high = count($array) - 1;
  $arr = $array;
  while ($low <= $high) {
  $mid = _intdiv(($low + $high), 2);
  if ($arr[$mid] == $item) {
  $arr = remove_at($arr, $mid);
  return $arr;
} else {
  if ($arr[$mid] < $item) {
  $low = $mid + 1;
} else {
  $high = $mid - 1;
};
}
};
  echo rtrim('ValueError: Either the item is not in the array or the array was unsorted'), PHP_EOL;
  return $arr;
};
  function binary_search_insert($array, $index) {
  global $nm, $im, $cont;
  $low = 0;
  $high = count($array) - 1;
  $arr = $array;
  while ($low <= $high) {
  $mid = _intdiv(($low + $high), 2);
  if ($arr[$mid] == $index) {
  $arr = insert_at($arr, $mid + 1, $index);
  return $arr;
} else {
  if ($arr[$mid] < $index) {
  $low = $mid + 1;
} else {
  $high = $mid - 1;
};
}
};
  $arr = insert_at($arr, $low, $index);
  return $arr;
};
  function change($cont, $idx, $num) {
  global $nm, $im;
  $numbermap = $cont['numbermap'];
  $indexmap = $cont['indexmap'];
  if (array_key_exists($idx, $indexmap)) {
  $old = $indexmap[$idx];
  $indexes = $numbermap[$old];
  if (count($indexes) == 1) {
  $numbermap[$old] = [];
} else {
  $numbermap[$old] = binary_search_delete($indexes, $idx);
};
}
  $indexmap[$idx] = $num;
  if (array_key_exists($num, $numbermap)) {
  $numbermap[$num] = binary_search_insert($numbermap[$num], $idx);
} else {
  $numbermap[$num] = [$idx];
}
  return ['numbermap' => $numbermap, 'indexmap' => $indexmap];
};
  function find($cont, $num) {
  global $nm, $im;
  $numbermap = $cont['numbermap'];
  if (array_key_exists($num, $numbermap)) {
  $arr = $numbermap[$num];
  if (count($arr) > 0) {
  return $arr[0];
};
}
  return -1;
};
  $nm = [];
  $im = [];
  $cont = ['numbermap' => $nm, 'indexmap' => $im];
  echo rtrim(json_encode(find($cont, 10), 1344)), PHP_EOL;
  $cont = change($cont, 0, 10);
  echo rtrim(json_encode(find($cont, 10), 1344)), PHP_EOL;
  $cont = change($cont, 0, 20);
  echo rtrim(json_encode(find($cont, 10), 1344)), PHP_EOL;
  echo rtrim(json_encode(find($cont, 20), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
