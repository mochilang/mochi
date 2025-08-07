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
  function encode($row, $col) {
  global $matrix;
  return _str($row) . ',' . _str($col);
};
  function is_safe($row, $col, $rows, $cols) {
  global $matrix;
  return $row >= 0 && $row < $rows && $col >= 0 && $col < $cols;
};
  function has($seen, $key) {
  global $matrix;
  return array_key_exists($key, $seen);
};
  function depth_first_search($row, $col, &$seen, $mat) {
  global $matrix;
  $rows = count($mat);
  $cols = count($mat[0]);
  $key = encode($row, $col);
  if (is_safe($row, $col, $rows, $cols) && (!has($seen, $key)) && $mat[$row][$col] == 1) {
  $seen[$key] = true;
  return 1 + depth_first_search($row + 1, $col, $seen, $mat) + depth_first_search($row - 1, $col, $seen, $mat) + depth_first_search($row, $col + 1, $seen, $mat) + depth_first_search($row, $col - 1, $seen, $mat);
} else {
  return 0;
}
};
  function find_max_area($mat) {
  global $matrix;
  $seen = [];
  $rows = count($mat);
  $max_area = 0;
  $r = 0;
  while ($r < $rows) {
  $line = $mat[$r];
  $cols = count($line);
  $c = 0;
  while ($c < $cols) {
  if ($line[$c] == 1) {
  $key = encode($r, $c);
  if (!(array_key_exists($key, $seen))) {
  $area = depth_first_search($r, $c, $seen, $mat);
  if ($area > $max_area) {
  $max_area = $area;
};
};
}
  $c = $c + 1;
};
  $r = $r + 1;
};
  return $max_area;
};
  $matrix = [[0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0], [0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0], [0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0], [0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0], [0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0]];
  echo rtrim(json_encode(find_max_area($matrix), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
