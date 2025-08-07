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
function _len($x) {
    if ($x === null) { return 0; }
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $seed = 123456789;
  function mochi_rand() {
  global $seed;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed;
};
  function rand_range($max) {
  global $seed;
  return fmod(mochi_rand(), $max);
};
  function mochi_shuffle(&$list_int) {
  global $seed;
  $i = count($list_int) - 1;
  while ($i > 0) {
  $j = rand_range($i + 1);
  $tmp = $list_int[$i];
  $list_int[$i] = $list_int[$j];
  $list_int[$j] = $tmp;
  $i = $i - 1;
};
  return $list_int;
};
  function rand_letter() {
  global $seed;
  $letters = 'abcdefghijklmnopqrstuvwxyz';
  $i = rand_range(26);
  return substr($letters, $i, $i + 1 - $i);
};
  function make_word_search($words, $width, $height) {
  global $seed;
  $board = [];
  $r = 0;
  while ($r < $height) {
  $row = [];
  $c = 0;
  while ($c < $width) {
  $row = _append($row, '');
  $c = $c + 1;
};
  $board = _append($board, $row);
  $r = $r + 1;
};
  return ['words' => $words, 'width' => $width, 'height' => $height, 'board' => $board];
};
  function insert_dir($ws, $word, $dr, $dc, $rows, $cols) {
  global $seed;
  $word_len = strlen($word);
  $ri = 0;
  while ($ri < count($rows)) {
  $row = $rows[$ri];
  $ci = 0;
  while ($ci < count($cols)) {
  $col = $cols[$ci];
  $end_r = $row + $dr * ($word_len - 1);
  $end_c = $col + $dc * ($word_len - 1);
  if ($end_r < 0 || $end_r >= $ws['height'] || $end_c < 0 || $end_c >= $ws['width']) {
  $ci = $ci + 1;
  continue;
}
  $k = 0;
  $ok = true;
  while ($k < $word_len) {
  $rr = $row + $dr * $k;
  $cc = $col + $dc * $k;
  if ($ws['board'][$rr][$cc] != '') {
  $ok = false;
  break;
}
  $k = $k + 1;
};
  if ($ok) {
  $k = 0;
  while ($k < $word_len) {
  $rr2 = $row + $dr * $k;
  $cc2 = $col + $dc * $k;
  $row_list = $ws['board'][$rr2];
  $row_list[$cc2] = substr($word, $k, $k + 1 - $k);
  $k = $k + 1;
};
  return true;
}
  $ci = $ci + 1;
};
  $ri = $ri + 1;
};
  return false;
};
  function generate_board($ws) {
  global $seed;
  $dirs_r = [-1, -1, 0, 1, 1, 1, 0, -1];
  $dirs_c = [0, 1, 1, 1, 0, -1, -1, -1];
  $i = 0;
  while ($i < _len($ws['words'])) {
  $word = $ws['words'][$i];
  $rows = [];
  $r = 0;
  while ($r < $ws['height']) {
  $rows = _append($rows, $r);
  $r = $r + 1;
};
  $cols = [];
  $c = 0;
  while ($c < $ws['width']) {
  $cols = _append($cols, $c);
  $c = $c + 1;
};
  $rows = mochi_shuffle($rows);
  $cols = mochi_shuffle($cols);
  $d = rand_range(8);
  insert_dir($ws, $word, $dirs_r[$d], $dirs_c[$d], $rows, $cols);
  $i = $i + 1;
};
};
  function visualise($ws, $add_fake_chars) {
  global $seed;
  $result = '';
  $r = 0;
  while ($r < $ws['height']) {
  $c = 0;
  while ($c < $ws['width']) {
  $ch = $ws['board'][$r][$c];
  if ($ch == '') {
  if ($add_fake_chars) {
  $ch = rand_letter();
} else {
  $ch = '#';
};
}
  $result = $result . $ch . ' ';
  $c = $c + 1;
};
  $result = $result . '
';
  $r = $r + 1;
};
  return $result;
};
  function main() {
  global $seed;
  $words = ['cat', 'dog', 'snake', 'fish'];
  $ws = make_word_search($words, 10, 10);
  generate_board($ws);
  echo rtrim(visualise($ws, true)), PHP_EOL;
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
