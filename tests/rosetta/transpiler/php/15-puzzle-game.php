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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $board = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0];
  $solved = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0];
  $empty = 15;
  $moves = 0;
  $quit = false;
  function randMove() {
  global $board, $solved, $empty, $moves, $quit;
  return _now() % 4;
};
  function isSolved() {
  global $board, $solved, $empty, $moves, $quit;
  $i = 0;
  while ($i < 16) {
  if ($board[$i] != $solved[$i]) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function isValidMove($m) {
  global $board, $solved, $empty, $moves, $quit;
  if ($m == 0) {
  return ['idx' => $empty - 4, 'ok' => _intdiv($empty, 4) > 0];
}
  if ($m == 1) {
  return ['idx' => $empty + 4, 'ok' => _intdiv($empty, 4) < 3];
}
  if ($m == 2) {
  return ['idx' => $empty + 1, 'ok' => $empty % 4 < 3];
}
  if ($m == 3) {
  return ['idx' => $empty - 1, 'ok' => $empty % 4 > 0];
}
  return ['idx' => 0, 'ok' => false];
};
  function doMove($m) {
  global $board, $solved, $empty, $moves, $quit;
  $r = isValidMove($m);
  if (!$r['ok']) {
  return false;
}
  $i = $empty;
  $j = $r['idx'];
  $tmp = $board[$i];
  $board[$i] = $board[$j];
  $board[$j] = $tmp;
  $empty = $j;
  $moves = $moves + 1;
  return true;
};
  function mochi_shuffle($n) {
  global $board, $solved, $empty, $moves, $quit;
  $i = 0;
  while ($i < $n || isSolved()) {
  if (doMove(randMove())) {
  $i = $i + 1;
}
};
};
  function printBoard() {
  global $board, $solved, $empty, $moves, $quit;
  $line = '';
  $i = 0;
  while ($i < 16) {
  $val = $board[$i];
  if ($val == 0) {
  $line = $line . '  .';
} else {
  $s = _str($val);
  if ($val < 10) {
  $line = $line . '  ' . $s;
} else {
  $line = $line . ' ' . $s;
};
}
  if ($i % 4 == 3) {
  echo rtrim($line), PHP_EOL;
  $line = '';
}
  $i = $i + 1;
};
};
  function playOneMove() {
  global $board, $solved, $empty, $moves, $quit;
  while (true) {
  echo rtrim('Enter move #' . _str($moves + 1) . ' (U, D, L, R, or Q): '), PHP_EOL;
  $s = trim(fgets(STDIN));
  if ($s == '') {
  continue;
}
  $c = substr($s, 0, 1 - 0);
  $m = 0;
  if ($c == 'U' || $c == 'u') {
  $m = 0;
} else {
  if ($c == 'D' || $c == 'd') {
  $m = 1;
} else {
  if ($c == 'R' || $c == 'r') {
  $m = 2;
} else {
  if ($c == 'L' || $c == 'l') {
  $m = 3;
} else {
  if ($c == 'Q' || $c == 'q') {
  echo rtrim('Quiting after ' . _str($moves) . ' moves.'), PHP_EOL;
  $quit = true;
  return;
} else {
  echo rtrim('Please enter "U", "D", "L", or "R" to move the empty cell
' . 'up, down, left, or right. You can also enter "Q" to quit.
' . 'Upper or lowercase is accepted and only the first non-blank
' . 'character is important (i.e. you may enter "up" if you like).'), PHP_EOL;
  continue;
};
};
};
};
}
  if (!doMove($m)) {
  echo rtrim('That is not a valid move at the moment.'), PHP_EOL;
  continue;
}
  return;
};
};
  function play() {
  global $board, $solved, $empty, $moves, $quit;
  echo rtrim('Starting board:'), PHP_EOL;
  while (!$quit && isSolved() == false) {
  echo rtrim(''), PHP_EOL;
  printBoard();
  playOneMove();
};
  if (isSolved()) {
  echo rtrim('You solved the puzzle in ' . _str($moves) . ' moves.'), PHP_EOL;
}
};
  function main() {
  global $board, $solved, $empty, $moves, $quit;
  mochi_shuffle(50);
  play();
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
