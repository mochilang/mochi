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
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        $q = bcdiv($sa, $sb, 0);
        $rem = bcmod($sa, $sb);
        $neg = ((strpos($sa, '-') === 0) xor (strpos($sb, '-') === 0));
        if ($neg && bccomp($rem, '0') != 0) {
            $q = bcsub($q, '1');
        }
        return intval($q);
    }
    $ai = intval($a);
    $bi = intval($b);
    $q = intdiv($ai, $bi);
    if ((($ai ^ $bi) < 0) && ($ai % $bi != 0)) {
        $q -= 1;
    }
    return $q;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function parent_index($child_idx) {
  global $heap, $m, $size;
  if ($child_idx > 0) {
  return _intdiv(($child_idx - 1), 2);
}
  return -1;
};
  function left_child_idx($parent_idx) {
  global $heap, $m, $size;
  return 2 * $parent_idx + 1;
};
  function right_child_idx($parent_idx) {
  global $heap, $m, $size;
  return 2 * $parent_idx + 2;
};
  function max_heapify(&$h, $heap_size, $index) {
  global $heap, $m, $size;
  $largest = $index;
  $left = left_child_idx($index);
  $right = right_child_idx($index);
  if ($left < $heap_size && $h[$left] > $h[$largest]) {
  $largest = $left;
}
  if ($right < $heap_size && $h[$right] > $h[$largest]) {
  $largest = $right;
}
  if ($largest != $index) {
  $temp = $h[$index];
  $h[$index] = $h[$largest];
  $h[$largest] = $temp;
  max_heapify($h, $heap_size, $largest);
}
};
  function build_max_heap(&$h) {
  global $heap, $m, $size;
  $heap_size = count($h);
  $i = _intdiv($heap_size, 2) - 1;
  while ($i >= 0) {
  max_heapify($h, $heap_size, $i);
  $i = $i - 1;
};
  return $heap_size;
};
  function extract_max(&$h, $heap_size) {
  global $heap, $m, $size;
  $max_value = $h[0];
  $h[0] = $h[$heap_size - 1];
  max_heapify($h, $heap_size - 1, 0);
  return $max_value;
};
  function insert(&$h, $heap_size, $value) {
  global $heap, $m, $size;
  if ($heap_size < count($h)) {
  $h[$heap_size] = $value;
} else {
  $h = _append($h, $value);
}
  $heap_size = $heap_size + 1;
  $idx = _intdiv(($heap_size - 1), 2);
  while ($idx >= 0) {
  max_heapify($h, $heap_size, $idx);
  $idx = _intdiv(($idx - 1), 2);
};
  return $heap_size;
};
  function heap_sort(&$h, $heap_size) {
  global $heap, $m;
  $size = $heap_size;
  $j = $size - 1;
  while ($j > 0) {
  $temp = $h[0];
  $h[0] = $h[$j];
  $h[$j] = $temp;
  $size = $size - 1;
  max_heapify($h, $size, 0);
  $j = $j - 1;
};
};
  function heap_to_string($h, $heap_size) {
  global $heap, $m, $size;
  $s = '[';
  $i = 0;
  while ($i < $heap_size) {
  $s = $s . _str($h[$i]);
  if ($i < $heap_size - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  $heap = [103.0, 9.0, 1.0, 7.0, 11.0, 15.0, 25.0, 201.0, 209.0, 107.0, 5.0];
  $size = build_max_heap($heap);
  echo rtrim(heap_to_string($heap, $size)), PHP_EOL;
  $m = extract_max($heap, $size);
  $size = $size - 1;
  echo rtrim(_str($m)), PHP_EOL;
  echo rtrim(heap_to_string($heap, $size)), PHP_EOL;
  $size = insert($heap, $size, 100.0);
  echo rtrim(heap_to_string($heap, $size)), PHP_EOL;
  heap_sort($heap, $size);
  echo rtrim(heap_to_string($heap, $size)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
