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
  $NUM_SQUARES = 9;
  $EMPTY_CELL = '.';
  function is_valid_sudoku_board($board) {
  global $NUM_SQUARES, $EMPTY_CELL, $valid_board, $invalid_board;
  if (count($board) != $NUM_SQUARES) {
  return false;
}
  $i = 0;
  while ($i < $NUM_SQUARES) {
  if (count($board[$i]) != $NUM_SQUARES) {
  return false;
}
  $i = $i + 1;
};
  $rows = [];
  $cols = [];
  $boxes = [];
  $i = 0;
  while ($i < $NUM_SQUARES) {
  $rows = _append($rows, []);
  $cols = _append($cols, []);
  $boxes = _append($boxes, []);
  $i = $i + 1;
};
  for ($r = 0; $r < $NUM_SQUARES; $r++) {
  for ($c = 0; $c < $NUM_SQUARES; $c++) {
  $value = $board[$r][$c];
  if ($value == $EMPTY_CELL) {
  continue;
}
  $box = intval(_intdiv($r, 3)) * 3 + intval(_intdiv($c, 3));
  if (in_array($value, $rows[$r]) || in_array($value, $cols[$c]) || in_array($value, $boxes[$box])) {
  return false;
}
  $rows[$r] = _append($rows[$r], $value);
  $cols[$c] = _append($cols[$c], $value);
  $boxes[$box] = _append($boxes[$box], $value);
};
};
  return true;
};
  $valid_board = [['5', '3', '.', '.', '7', '.', '.', '.', '.'], ['6', '.', '.', '1', '9', '5', '.', '.', '.'], ['.', '9', '8', '.', '.', '.', '.', '6', '.'], ['8', '.', '.', '.', '6', '.', '.', '.', '3'], ['4', '.', '.', '8', '.', '3', '.', '.', '1'], ['7', '.', '.', '.', '2', '.', '.', '.', '6'], ['.', '6', '.', '.', '.', '.', '2', '8', '.'], ['.', '.', '.', '4', '1', '9', '.', '.', '5'], ['.', '.', '.', '.', '8', '.', '.', '7', '9']];
  $invalid_board = [['8', '3', '.', '.', '7', '.', '.', '.', '.'], ['6', '.', '.', '1', '9', '5', '.', '.', '.'], ['.', '9', '8', '.', '.', '.', '.', '6', '.'], ['8', '.', '.', '.', '6', '.', '.', '.', '3'], ['4', '.', '.', '8', '.', '3', '.', '.', '1'], ['7', '.', '.', '.', '2', '.', '.', '.', '6'], ['.', '6', '.', '.', '.', '.', '2', '8', '.'], ['.', '.', '.', '4', '1', '9', '.', '.', '5'], ['.', '.', '.', '.', '8', '.', '.', '7', '9']];
  echo rtrim(json_encode(is_valid_sudoku_board($valid_board), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_valid_sudoku_board($invalid_board), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
