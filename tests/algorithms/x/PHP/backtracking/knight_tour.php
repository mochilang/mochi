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
  function get_valid_pos($position, $n) {
  global $board;
  $y = $position[0];
  $x = $position[1];
  $positions = [[$y + 1, $x + 2], [$y - 1, $x + 2], [$y + 1, $x - 2], [$y - 1, $x - 2], [$y + 2, $x + 1], [$y + 2, $x - 1], [$y - 2, $x + 1], [$y - 2, $x - 1]];
  $permissible = [];
  for ($idx = 0; $idx < count($positions); $idx++) {
  $inner = $positions[$idx];
  $y_test = $inner[0];
  $x_test = $inner[1];
  if ($y_test >= 0 && $y_test < $n && $x_test >= 0 && $x_test < $n) {
  $permissible = _append($permissible, $inner);
}
};
  return $permissible;
};
  function is_complete($board) {
  for ($i = 0; $i < count($board); $i++) {
  $row = $board[$i];
  for ($j = 0; $j < count($row); $j++) {
  if ($row[$j] == 0) {
  return false;
}
};
};
  return true;
};
  function open_knight_tour_helper(&$board, $pos, $curr) {
  if (is_complete($board)) {
  return true;
}
  $moves = get_valid_pos($pos, count($board));
  for ($i = 0; $i < count($moves); $i++) {
  $position = $moves[$i];
  $y = $position[0];
  $x = $position[1];
  if ($board[$y][$x] == 0) {
  $board[$y][$x] = $curr + 1;
  if (open_knight_tour_helper($board, $position, $curr + 1)) {
  return true;
};
  $board[$y][$x] = 0;
}
};
  return false;
};
  function open_knight_tour($n) {
  $board = [];
  for ($i = 0; $i < $n; $i++) {
  $row = [];
  for ($j = 0; $j < $n; $j++) {
  $row = _append($row, 0);
};
  $board = _append($board, $row);
};
  for ($i = 0; $i < $n; $i++) {
  for ($j = 0; $j < $n; $j++) {
  $board[$i][$j] = 1;
  if (open_knight_tour_helper($board, [$i, $j], 1)) {
  return $board;
}
  $board[$i][$j] = 0;
};
};
  echo rtrim('Open Knight Tour cannot be performed on a board of size ' . _str($n)), PHP_EOL;
  return $board;
};
  $board = open_knight_tour(1);
  echo rtrim(json_encode($board[0][0], 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
