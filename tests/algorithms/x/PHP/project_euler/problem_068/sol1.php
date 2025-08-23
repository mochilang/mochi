<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
  function range_desc($start, $end) {
  $res = [];
  $i = $start;
  while ($i >= $end) {
  $res = _append($res, $i);
  $i = $i - 1;
};
  return $res;
};
  function range_asc($start, $end) {
  $res = [];
  $i = $start;
  while ($i <= $end) {
  $res = _append($res, $i);
  $i = $i + 1;
};
  return $res;
};
  function concat_lists($a, $b) {
  $res = $a;
  $i = 0;
  while ($i < count($b)) {
  $res = _append($res, $b[$i]);
  $i = $i + 1;
};
  return $res;
};
  function swap($xs, $i, $j) {
  $res = [];
  $k = 0;
  while ($k < count($xs)) {
  if ($k == $i) {
  $res = _append($res, $xs[$j]);
} else {
  if ($k == $j) {
  $res = _append($res, $xs[$i]);
} else {
  $res = _append($res, $xs[$k]);
};
}
  $k = $k + 1;
};
  return $res;
};
  function generate_gon_ring($gon_side, $perm) {
  $result = [];
  $result = _append($result, $perm[0]);
  $result = _append($result, $perm[1]);
  $result = _append($result, $perm[2]);
  $extended = _append($perm, $perm[1]);
  $magic_number = ($gon_side < 5 ? 1 : 2);
  $i = 1;
  while ($i < count($extended) / 3 + $magic_number) {
  $result = _append($result, $extended[2 * $i + 1]);
  $result = _append($result, $result[3 * $i - 1]);
  $result = _append($result, $extended[2 * $i + 2]);
  $i = $i + 1;
};
  return $result;
};
  function min_outer($numbers) {
  $min_val = $numbers[0];
  $i = 3;
  while ($i < count($numbers)) {
  if ($numbers[$i] < $min_val) {
  $min_val = $numbers[$i];
}
  $i = $i + 3;
};
  return $min_val;
};
  function is_magic_gon($numbers) {
  if (fmod(count($numbers), 3) != 0) {
  return false;
}
  if (min_outer($numbers) != $numbers[0]) {
  return false;
}
  $total = $numbers[0] + $numbers[1] + $numbers[2];
  $i = 3;
  while ($i < count($numbers)) {
  if ($numbers[$i] + $numbers[$i + 1] + $numbers[$i + 2] != $total) {
  return false;
}
  $i = $i + 3;
};
  return true;
};
  function permute_search($nums, $start, $gon_side, $current_max) {
  if ($start == count($nums)) {
  $ring = generate_gon_ring($gon_side, $nums);
  if (is_magic_gon($ring)) {
  $s = '';
  $k = 0;
  while ($k < count($ring)) {
  $s = $s . _str($ring[$k]);
  $k = $k + 1;
};
  if ($s > $current_max) {
  return $s;
};
};
  return $current_max;
}
  $res = $current_max;
  $i = $start;
  while ($i < count($nums)) {
  $swapped = swap($nums, $start, $i);
  $candidate = permute_search($swapped, $start + 1, $gon_side, $res);
  if ($candidate > $res) {
  $res = $candidate;
}
  $i = $i + 1;
};
  return $res;
};
  function solution($gon_side) {
  if ($gon_side < 3 || $gon_side > 5) {
  return '';
}
  $small = range_desc($gon_side + 1, 1);
  $big = range_asc($gon_side + 2, $gon_side * 2);
  $numbers = concat_lists($small, $big);
  $max_str = permute_search($numbers, 0, $gon_side, '');
  return $max_str;
};
  echo rtrim(solution(5)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
