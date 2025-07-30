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
$__start_mem = memory_get_usage();
$__start = _now();
  $rows = 20;
  $cols = 30;
  $p = 0.01;
  $f = 0.001;
  function mochi_repeat($ch, $n) {
  global $rows, $cols, $p, $f, $board;
  $s = '';
  $i = 0;
  while ($i < $n) {
  $s = $s . $ch;
  $i = $i + 1;
};
  return $s;
};
  function chance($prob) {
  global $rows, $cols, $p, $f, $board;
  $threshold = intval($prob * 1000.0);
  return fmod(_now(), 1000) < $threshold;
};
  function newBoard() {
  global $rows, $cols, $p, $f, $board;
  $b = [];
  $r = 0;
  while ($r < $rows) {
  $row = [];
  $c = 0;
  while ($c < $cols) {
  if (fmod(_now(), 2) == 0) {
  $row = array_merge($row, ['T']);
} else {
  $row = array_merge($row, [' ']);
}
  $c = $c + 1;
};
  $b = array_merge($b, [$row]);
  $r = $r + 1;
};
  return $b;
};
  function step($src) {
  global $rows, $cols, $p, $f, $board;
  $dst = [];
  $r = 0;
  while ($r < $rows) {
  $row = [];
  $c = 0;
  while ($c < $cols) {
  $cell = $src[$r][$c];
  $next = $cell;
  if ($cell == '#') {
  $next = ' ';
} else {
  if ($cell == 'T') {
  $burning = false;
  $dr = -1;
  while ($dr <= 1) {
  $dc = -1;
  while ($dc <= 1) {
  if ($dr != 0 || $dc != 0) {
  $rr = $r + $dr;
  $cc = $c + $dc;
  if ($rr >= 0 && $rr < $rows && $cc >= 0 && $cc < $cols) {
  if ($src[$rr][$cc] == '#') {
  $burning = true;
};
};
}
  $dc = $dc + 1;
};
  $dr = $dr + 1;
};
  if ($burning || chance($f)) {
  $next = '#';
};
} else {
  if (chance($p)) {
  $next = 'T';
};
};
}
  $row = array_merge($row, [$next]);
  $c = $c + 1;
};
  $dst = array_merge($dst, [$row]);
  $r = $r + 1;
};
  return $dst;
};
  function printBoard($b) {
  global $rows, $cols, $p, $f, $board;
  echo rtrim(repeat('__', $cols) . '

'), PHP_EOL;
  $r = 0;
  while ($r < $rows) {
  $line = '';
  $c = 0;
  while ($c < $cols) {
  $cell = $b[$r][$c];
  if ($cell == ' ') {
  $line = $line . '  ';
} else {
  $line = $line . ' ' . $cell;
}
  $c = $c + 1;
};
  echo rtrim($line . '
'), PHP_EOL;
  $r = $r + 1;
};
};
  $board = newBoard();
  printBoard($board);
  $board = step($board);
  printBoard($board);
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
