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
  function randInt($seed, $n) {
  $next = ($seed * 1664525 + 1013904223) % 2147483647;
  return [$next, $next % $n];
};
  function newBoard($n, $seed) {
  $board = [];
  $s = $seed;
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $r = randInt($s, 2);
  $s = $r[0];
  $row = array_merge($row, [$r[1]]);
  $j = $j + 1;
};
  $board = array_merge($board, [$row]);
  $i = $i + 1;
};
  return [$board, $s];
};
  function copyBoard($b) {
  $nb = [];
  $i = 0;
  while ($i < count($b)) {
  $row = [];
  $j = 0;
  while ($j < count($b[$i])) {
  $row = array_merge($row, [$b[$i][$j]]);
  $j = $j + 1;
};
  $nb = array_merge($nb, [$row]);
  $i = $i + 1;
};
  return $nb;
};
  function flipRow(&$b, $r) {
  $j = 0;
  while ($j < count($b[$r])) {
  $b[$r][$j] = 1 - $b[$r][$j];
  $j = $j + 1;
};
  return $b;
};
  function flipCol(&$b, $c) {
  $i = 0;
  while ($i < count($b)) {
  $b[$i][$c] = 1 - $b[$i][$c];
  $i = $i + 1;
};
  return $b;
};
  function boardsEqual($a, $b) {
  $i = 0;
  while ($i < count($a)) {
  $j = 0;
  while ($j < count($a[$i])) {
  if ($a[$i][$j] != $b[$i][$j]) {
  return false;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return true;
};
  function shuffleBoard($b, $seed) {
  $s = $seed;
  $n = count($b);
  $k = 0;
  while ($k < 2 * $n) {
  $r = randInt($s, $n);
  $s = $r[0];
  $idx = intval($r[1]);
  if ($k % 2 == 0) {
  $b = flipRow($b, $idx);
} else {
  $b = flipCol($b, $idx);
}
  $k = $k + 1;
};
  return [$b, $s];
};
  function solve($board, $target) {
  $n = count($board);
  $row = [];
  $col = [];
  $i = 0;
  while ($i < $n) {
  $diff = ($board[$i][0] != $target[$i][0] ? 1 : 0);
  $row = array_merge($row, [$diff]);
  $i = $i + 1;
};
  $j = 0;
  while ($j < $n) {
  $diff = ($board[0][$j] != $target[0][$j] ? 1 : 0);
  $val = fmod(($diff + $row[0]), 2);
  $col = array_merge($col, [$val]);
  $j = $j + 1;
};
  return ['row' => $row, 'col' => $col];
};
  function applySolution($b, $sol) {
  $board = $b;
  $moves = 0;
  $i = 0;
  while ($i < count($sol['row'])) {
  if ($sol['row'][$i] == 1) {
  $board = flipRow($board, $i);
  $moves = $moves + 1;
}
  $i = $i + 1;
};
  $j = 0;
  while ($j < count($sol['col'])) {
  if ($sol['col'][$j] == 1) {
  $board = flipCol($board, $j);
  $moves = $moves + 1;
}
  $j = $j + 1;
};
  return [$board, $moves];
};
  function printBoard($b) {
  $i = 0;
  while ($i < count($b)) {
  $line = '';
  $j = 0;
  while ($j < count($b[$i])) {
  $line = $line . _str($b[$i][$j]);
  if ($j < count($b[$i]) - 1) {
  $line = $line . ' ';
}
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
};
  function main() {
  $n = 3;
  $seed = 1;
  $res = newBoard($n, $seed);
  $target = $res[0];
  $seed = intval($res[1]);
  $board = copyBoard($target);
  while (true) {
  $sres = shuffleBoard(copyBoard($board), $seed);
  $board = $sres[0];
  $seed = intval($sres[1]);
  if (!boardsEqual($board, $target)) {
  break;
}
};
  echo rtrim('Target:'), PHP_EOL;
  printBoard($target);
  echo rtrim('Board:'), PHP_EOL;
  printBoard($board);
  $sol = solve($board, $target);
  $ares = applySolution($board, $sol);
  $board = $ares[0];
  $moves = intval($ares[1]);
  echo rtrim('Solved:'), PHP_EOL;
  printBoard($board);
  echo rtrim('Moves: ' . _str($moves)), PHP_EOL;
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
