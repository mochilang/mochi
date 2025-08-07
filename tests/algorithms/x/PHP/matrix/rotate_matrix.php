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
  function abs_int($n) {
  global $mat, $r90, $r180, $r270;
  if ($n < 0) {
  return -$n;
}
  return $n;
};
  function make_matrix($row_size) {
  global $r90, $r180, $r270;
  $size = abs_int($row_size);
  if ($size == 0) {
  $size = 4;
}
  $mat = [];
  $y = 0;
  while ($y < $size) {
  $row = [];
  $x = 0;
  while ($x < $size) {
  $row = _append($row, 1 + $x + $y * $size);
  $x = $x + 1;
};
  $mat = _append($mat, $row);
  $y = $y + 1;
};
  return $mat;
};
  function transpose($mat) {
  global $r90, $r180, $r270;
  $n = count($mat);
  $result = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = _append($row, $mat[$j][$i]);
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
};
  function reverse_row($mat) {
  global $r90, $r180, $r270;
  $result = [];
  $i = count($mat) - 1;
  while ($i >= 0) {
  $result = _append($result, $mat[$i]);
  $i = $i - 1;
};
  return $result;
};
  function reverse_column($mat) {
  global $r90, $r180, $r270;
  $result = [];
  $i = 0;
  while ($i < count($mat)) {
  $row = [];
  $j = count($mat[$i]) - 1;
  while ($j >= 0) {
  $row = _append($row, $mat[$i][$j]);
  $j = $j - 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
};
  function rotate_90($mat) {
  global $r90, $r180, $r270;
  $t = transpose($mat);
  $rr = reverse_row($t);
  return $rr;
};
  function rotate_180($mat) {
  global $r90, $r180, $r270;
  $rc = reverse_column($mat);
  $rr = reverse_row($rc);
  return $rr;
};
  function rotate_270($mat) {
  global $r90, $r180, $r270;
  $t = transpose($mat);
  $rc = reverse_column($t);
  return $rc;
};
  function row_to_string($row) {
  global $mat, $r90, $r180, $r270;
  $line = '';
  $i = 0;
  while ($i < count($row)) {
  if ($i == 0) {
  $line = _str($row[$i]);
} else {
  $line = $line . ' ' . _str($row[$i]);
}
  $i = $i + 1;
};
  return $line;
};
  function print_matrix($mat) {
  global $r90, $r180, $r270;
  $i = 0;
  while ($i < count($mat)) {
  echo rtrim(row_to_string($mat[$i])), PHP_EOL;
  $i = $i + 1;
};
};
  $mat = make_matrix(4);
  echo rtrim('
origin:
'), PHP_EOL;
  print_matrix($mat);
  echo rtrim('
rotate 90 counterclockwise:
'), PHP_EOL;
  $r90 = rotate_90($mat);
  print_matrix($r90);
  $mat = make_matrix(4);
  echo rtrim('
origin:
'), PHP_EOL;
  print_matrix($mat);
  echo rtrim('
rotate 180:
'), PHP_EOL;
  $r180 = rotate_180($mat);
  print_matrix($r180);
  $mat = make_matrix(4);
  echo rtrim('
origin:
'), PHP_EOL;
  print_matrix($mat);
  echo rtrim('
rotate 270 counterclockwise:
'), PHP_EOL;
  $r270 = rotate_270($mat);
  print_matrix($r270);
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
