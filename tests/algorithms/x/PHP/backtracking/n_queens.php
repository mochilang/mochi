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
  function create_board($n) {
  $board = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = _append($row, 0);
  $j = $j + 1;
};
  $board = _append($board, $row);
  $i = $i + 1;
};
  return $board;
};
  function is_safe($board, $row, $column) {
  $n = count($board);
  $i = 0;
  while ($i < $row) {
  if ($board[$i][$column] == 1) {
  return false;
}
  $i = $i + 1;
};
  $i = $row - 1;
  $j = $column - 1;
  while ($i >= 0 && $j >= 0) {
  if ($board[$i][$j] == 1) {
  return false;
}
  $i = $i - 1;
  $j = $j - 1;
};
  $i = $row - 1;
  $j = $column + 1;
  while ($i >= 0 && $j < $n) {
  if ($board[$i][$j] == 1) {
  return false;
}
  $i = $i - 1;
  $j = $j + 1;
};
  return true;
};
  function row_string($row) {
  $s = '';
  $j = 0;
  while ($j < count($row)) {
  if ($row[$j] == 1) {
  $s = $s . 'Q ';
} else {
  $s = $s . '. ';
}
  $j = $j + 1;
};
  return $s;
};
  function printboard($board) {
  $i = 0;
  while ($i < count($board)) {
  echo rtrim(row_string($board[$i])), PHP_EOL;
  $i = $i + 1;
};
};
  function solve(&$board, $row) {
  if ($row >= count($board)) {
  printboard($board);
  echo rtrim(''), PHP_EOL;
  return 1;
}
  $count = 0;
  $i = 0;
  while ($i < count($board)) {
  if (is_safe($board, $row, $i)) {
  $board[$row][$i] = 1;
  $count = $count + solve($board, $row + 1);
  $board[$row][$i] = 0;
}
  $i = $i + 1;
};
  return $count;
};
  function n_queens($n) {
  $board = create_board($n);
  $total = solve($board, 0);
  echo rtrim('The total number of solutions are: ' . _str($total)), PHP_EOL;
  return $total;
};
  n_queens(4);
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
