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
$__start_mem = memory_get_usage();
$__start = _now();
  function search_in_sorted_matrix($mat, $m, $n, $key) {
  $i = $m - 1;
  $j = 0;
  while ($i >= 0 && $j < $n) {
  if ($key == $mat[$i][$j]) {
  echo rtrim('Key ' . _str($key) . ' found at row- ' . _str($i + 1) . ' column- ' . _str($j + 1)), PHP_EOL;
  return;
}
  if ($key < $mat[$i][$j]) {
  $i = $i - 1;
} else {
  $j = $j + 1;
}
};
  echo rtrim('Key ' . _str($key) . ' not found'), PHP_EOL;
};
  function main() {
  $mat = [[2.0, 5.0, 7.0], [4.0, 8.0, 13.0], [9.0, 11.0, 15.0], [12.0, 17.0, 20.0]];
  search_in_sorted_matrix($mat, count($mat), count($mat[0]), 5.0);
  search_in_sorted_matrix($mat, count($mat), count($mat[0]), 21.0);
  $mat2 = [[2.1, 5.0, 7.0], [4.0, 8.0, 13.0], [9.0, 11.0, 15.0], [12.0, 17.0, 20.0]];
  search_in_sorted_matrix($mat2, count($mat2), count($mat2[0]), 2.1);
  search_in_sorted_matrix($mat2, count($mat2), count($mat2[0]), 2.2);
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
