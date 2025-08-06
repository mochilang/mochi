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
$__start_mem = memory_get_usage();
$__start = _now();
  function encrypt($input_string, $key) {
  global $bf;
  if ($key <= 0) {
  $panic('Height of grid can\'t be 0 or negative');
}
  if ($key == 1 || strlen($input_string) <= $key) {
  return $input_string;
}
  $lowest = $key - 1;
  $temp_grid = [];
  $i = 0;
  while ($i < $key) {
  $temp_grid = _append($temp_grid, []);
  $i = $i + 1;
};
  $position = 0;
  while ($position < strlen($input_string)) {
  $num = $position % ($lowest * 2);
  $alt = $lowest * 2 - $num;
  if ($num > $alt) {
  $num = $alt;
}
  $row = $temp_grid[$num];
  $row = _append($row, $input_string[$position]);
  $temp_grid[$num] = $row;
  $position = $position + 1;
};
  $output = '';
  $i = 0;
  while ($i < $key) {
  $row = $temp_grid[$i];
  $j = 0;
  while ($j < count($row)) {
  $output = $output . $row[$j];
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $output;
};
  function decrypt($input_string, $key) {
  global $bf;
  if ($key <= 0) {
  $panic('Height of grid can\'t be 0 or negative');
}
  if ($key == 1) {
  return $input_string;
}
  $lowest = $key - 1;
  $counts = [];
  $i = 0;
  while ($i < $key) {
  $counts = _append($counts, 0);
  $i = $i + 1;
};
  $pos = 0;
  while ($pos < strlen($input_string)) {
  $num = $pos % ($lowest * 2);
  $alt = $lowest * 2 - $num;
  if ($num > $alt) {
  $num = $alt;
}
  $counts[$num] = $counts[$num] + 1;
  $pos = $pos + 1;
};
  $grid = [];
  $counter = 0;
  $i = 0;
  while ($i < $key) {
  $length = $counts[$i];
  $slice = substr($input_string, $counter, $counter + $length - $counter);
  $row = [];
  $j = 0;
  while ($j < strlen($slice)) {
  $row = _append($row, $slice[$j]);
  $j = $j + 1;
};
  $grid = _append($grid, $row);
  $counter = $counter + $length;
  $i = $i + 1;
};
  $indices = [];
  $i = 0;
  while ($i < $key) {
  $indices = _append($indices, 0);
  $i = $i + 1;
};
  $output = '';
  $pos = 0;
  while ($pos < strlen($input_string)) {
  $num = $pos % ($lowest * 2);
  $alt = $lowest * 2 - $num;
  if ($num > $alt) {
  $num = $alt;
}
  $output = $output . $grid[$num][$indices[$num]];
  $indices[$num] = $indices[$num] + 1;
  $pos = $pos + 1;
};
  return $output;
};
  function bruteforce($input_string) {
  global $bf;
  $results = [];
  $key_guess = 1;
  while ($key_guess < strlen($input_string)) {
  $results[$key_guess] = decrypt($input_string, $key_guess);
  $key_guess = $key_guess + 1;
};
  return $results;
};
  echo rtrim(encrypt('Hello World', 4)), PHP_EOL;
  echo rtrim(decrypt('HWe olordll', 4)), PHP_EOL;
  $bf = bruteforce('HWe olordll');
  echo rtrim($bf[4]), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
