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
function repeat($s, $n) {
    return str_repeat($s, intval($n));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function contains($xs, $x) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function mochi_repeat($s, $times) {
  $result = '';
  $i = 0;
  while ($i < $times) {
  $result = $result . $s;
  $i = $i + 1;
};
  return $result;
};
  function build_board($pos, $n) {
  $board = [];
  $i = 0;
  while ($i < count($pos)) {
  $col = $pos[$i];
  $line = repeat('. ', $col) . 'Q ' . repeat('. ', $n - 1 - $col);
  $board = _append($board, $line);
  $i = $i + 1;
};
  return $board;
};
  function depth_first_search($pos, $dr, $dl, $n) {
  $row = count($pos);
  if ($row == $n) {
  $single = [];
  $single = _append($single, build_board($pos, $n));
  return $single;
}
  $boards = [];
  $col = 0;
  while ($col < $n) {
  if (in_array($col, $pos) || in_array($row - $col, $dr) || in_array($row + $col, $dl)) {
  $col = $col + 1;
  continue;
}
  $result = depth_first_search(_append($pos, $col), _append($dr, $row - $col), _append($dl, $row + $col), $n);
  $boards = array_merge($boards, $result);
  $col = $col + 1;
};
  return $boards;
};
  function n_queens_solution($n) {
  $boards = depth_first_search([], [], [], $n);
  $i = 0;
  while ($i < count($boards)) {
  $j = 0;
  while ($j < count($boards[$i])) {
  echo rtrim($boards[$i][$j]), PHP_EOL;
  $j = $j + 1;
};
  echo rtrim(''), PHP_EOL;
  $i = $i + 1;
};
  echo rtrim(json_encode(count($boards), 1344)) . " " . rtrim('solutions were found.'), PHP_EOL;
  return count($boards);
};
  n_queens_solution(4);
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
