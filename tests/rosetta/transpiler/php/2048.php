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
  $SIZE = 4;
  function newBoard() {
  global $SIZE, $board, $r, $full, $score, $cmd, $moved, $m, $r2;
  $b = [];
  $y = 0;
  while ($y < $SIZE) {
  $row = [];
  $x = 0;
  while ($x < $SIZE) {
  $row = array_merge($row, [0]);
  $x = $x + 1;
};
  $b = array_merge($b, [$row]);
  $y = $y + 1;
};
  return ['cells' => $b];
};
  function spawnTile($b) {
  global $SIZE, $board, $r, $full, $score, $cmd, $moved, $m, $r2;
  $grid = $b['cells'];
  $empty = [];
  $y = 0;
  while ($y < $SIZE) {
  $x = 0;
  while ($x < $SIZE) {
  if ($grid[$y][$x] == 0) {
  $empty = array_merge($empty, [[$x, $y]]);
}
  $x = $x + 1;
};
  $y = $y + 1;
};
  if (count($empty) == 0) {
  return ['board' => $b, 'full' => true];
}
  $idx = fmod(_now(), count($empty));
  $cell = $empty[$idx];
  $val = 4;
  if (fmod(_now(), 10) < 9) {
  $val = 2;
}
  $grid[$cell[1]][$cell[0]] = $val;
  return ['board' => ['cells' => $grid], 'full' => count($empty) == 1];
};
  function pad($n) {
  global $SIZE, $board, $r, $full, $score, $cmd, $moved, $m, $r2;
  $s = _str($n);
  $pad = 4 - strlen($s);
  $i = 0;
  $out = '';
  while ($i < 'pad') {
  $out = $out . ' ';
  $i = $i + 1;
};
  return $out . $s;
};
  function draw($b, $score) {
  global $SIZE, $board, $r, $full, $cmd, $moved, $m, $r2;
  echo rtrim('Score: ' . _str($score)), PHP_EOL;
  $y = 0;
  while ($y < $SIZE) {
  echo rtrim('+----+----+----+----+'), PHP_EOL;
  $line = '|';
  $x = 0;
  while ($x < $SIZE) {
  $v = $b['cells'][$y][$x];
  if ($v == 0) {
  $line = $line . '    |';
} else {
  $line = $line . pad($v) . '|';
}
  $x = $x + 1;
};
  echo rtrim($line), PHP_EOL;
  $y = $y + 1;
};
  echo rtrim('+----+----+----+----+'), PHP_EOL;
  echo rtrim('W=Up S=Down A=Left D=Right Q=Quit'), PHP_EOL;
};
  function reverseRow($r) {
  global $SIZE, $board, $full, $score, $cmd, $moved, $m, $r2;
  $out = [];
  $i = count($r) - 1;
  while ($i >= 0) {
  $out = array_merge($out, [$r[$i]]);
  $i = $i - 1;
};
  return $out;
};
  function slideLeft($row) {
  global $SIZE, $board, $r, $full, $score, $cmd, $moved, $m, $r2;
  $xs = [];
  $i = 0;
  while ($i < count($row)) {
  if ($row[$i] != 0) {
  $xs = array_merge($xs, [$row[$i]]);
}
  $i = $i + 1;
};
  $res = [];
  $gain = 0;
  $i = 0;
  while ($i < count($xs)) {
  if ($i + 1 < count($xs) && $xs[$i] == $xs[$i + 1]) {
  $v = $xs[$i] * 2;
  $gain = $gain + $v;
  $res = array_merge($res, [$v]);
  $i = $i + 2;
} else {
  $res = array_merge($res, [$xs[$i]]);
  $i = $i + 1;
}
};
  while (count($res) < $SIZE) {
  $res = array_merge($res, [0]);
};
  return ['row' => $res, 'gain' => $gain];
};
  function moveLeft($b, $score) {
  global $SIZE, $board, $full, $cmd, $m, $r2;
  $grid = $b['cells'];
  $moved = false;
  $y = 0;
  while ($y < $SIZE) {
  $r = slideLeft($grid[$y]);
  $new = $r['row'];
  $score = $score + $r['gain'];
  $x = 0;
  while ($x < $SIZE) {
  if ($grid[$y][$x] != $new[$x]) {
  $moved = true;
}
  $grid[$y][$x] = $new[$x];
  $x = $x + 1;
};
  $y = $y + 1;
};
  return ['board' => ['cells' => $grid], 'score' => $score, 'moved' => $moved];
};
  function moveRight($b, $score) {
  global $SIZE, $board, $full, $cmd, $m, $r2;
  $grid = $b['cells'];
  $moved = false;
  $y = 0;
  while ($y < $SIZE) {
  $rev = reverseRow($grid[$y]);
  $r = slideLeft($rev);
  $rev = $r['row'];
  $score = $score + $r['gain'];
  $rev = reverseRow($rev);
  $x = 0;
  while ($x < $SIZE) {
  if ($grid[$y][$x] != $rev[$x]) {
  $moved = true;
}
  $grid[$y][$x] = $rev[$x];
  $x = $x + 1;
};
  $y = $y + 1;
};
  return ['board' => ['cells' => $grid], 'score' => $score, 'moved' => $moved];
};
  function getCol($b, $x) {
  global $SIZE, $board, $r, $full, $score, $cmd, $moved, $m, $r2;
  $col = [];
  $y = 0;
  while ($y < $SIZE) {
  $col = array_merge($col, [$b['cells'][$y][$x]]);
  $y = $y + 1;
};
  return $col;
};
  function setCol(&$b, $x, $col) {
  global $SIZE, $board, $r, $full, $score, $cmd, $moved, $m, $r2;
  $rows = $b['cells'];
  $y = 0;
  while ($y < $SIZE) {
  $row = $rows[$y];
  $row[$x] = $col[$y];
  $rows[$y] = $row;
  $y = $y + 1;
};
  $b['cells'] = $rows;
};
  function moveUp($b, $score) {
  global $SIZE, $board, $full, $cmd, $m, $r2;
  $grid = $b['cells'];
  $moved = false;
  $x = 0;
  while ($x < $SIZE) {
  $col = getCol($b, $x);
  $r = slideLeft($col);
  $new = $r['row'];
  $score = $score + $r['gain'];
  $y = 0;
  while ($y < $SIZE) {
  if ($grid[$y][$x] != $new[$y]) {
  $moved = true;
}
  $grid[$y][$x] = $new[$y];
  $y = $y + 1;
};
  $x = $x + 1;
};
  return ['board' => ['cells' => $grid], 'score' => $score, 'moved' => $moved];
};
  function moveDown($b, $score) {
  global $SIZE, $board, $full, $cmd, $m, $r2;
  $grid = $b['cells'];
  $moved = false;
  $x = 0;
  while ($x < $SIZE) {
  $col = reverseRow(getCol($b, $x));
  $r = slideLeft($col);
  $col = $r['row'];
  $score = $score + $r['gain'];
  $col = reverseRow($col);
  $y = 0;
  while ($y < $SIZE) {
  if ($grid[$y][$x] != $col[$y]) {
  $moved = true;
}
  $grid[$y][$x] = $col[$y];
  $y = $y + 1;
};
  $x = $x + 1;
};
  return ['board' => ['cells' => $grid], 'score' => $score, 'moved' => $moved];
};
  function hasMoves($b) {
  global $SIZE, $board, $r, $full, $score, $cmd, $moved, $m, $r2;
  $y = 0;
  while ($y < $SIZE) {
  $x = 0;
  while ($x < $SIZE) {
  if ($b['cells'][$y][$x] == 0) {
  return true;
}
  if ($x + 1 < $SIZE && $b['cells'][$y][$x] == $b['cells'][$y][$x + 1]) {
  return true;
}
  if ($y + 1 < $SIZE && $b['cells'][$y][$x] == $b['cells'][$y + 1][$x]) {
  return true;
}
  $x = $x + 1;
};
  $y = $y + 1;
};
  return false;
};
  function has2048($b) {
  global $SIZE, $board, $r, $full, $score, $cmd, $moved, $m, $r2;
  $y = 0;
  while ($y < $SIZE) {
  $x = 0;
  while ($x < $SIZE) {
  if ($b['cells'][$y][$x] >= 2048) {
  return true;
}
  $x = $x + 1;
};
  $y = $y + 1;
};
  return false;
};
  $board = newBoard();
  $r = spawnTile($board);
  $board = $r['board'];
  $full = $r['full'];
  $r = spawnTile($board);
  $board = $r['board'];
  $full = $r['full'];
  $score = 0;
  draw($board, $score);
  while (true) {
  echo rtrim('Move: '), PHP_EOL;
  $cmd = trim(fgets(STDIN));
  $moved = false;
  if ($cmd == 'a' || $cmd == 'A') {
  $m = moveLeft($board, $score);
  $board = $m['board'];
  $score = $m['score'];
  $moved = $m['moved'];
}
  if ($cmd == 'd' || $cmd == 'D') {
  $m = moveRight($board, $score);
  $board = $m['board'];
  $score = $m['score'];
  $moved = $m['moved'];
}
  if ($cmd == 'w' || $cmd == 'W') {
  $m = moveUp($board, $score);
  $board = $m['board'];
  $score = $m['score'];
  $moved = $m['moved'];
}
  if ($cmd == 's' || $cmd == 'S') {
  $m = moveDown($board, $score);
  $board = $m['board'];
  $score = $m['score'];
  $moved = $m['moved'];
}
  if ($cmd == 'q' || $cmd == 'Q') {
  break;
}
  if ($moved) {
  $r2 = spawnTile($board);
  $board = $r2['board'];
  $full = $r2['full'];
  if ($full && (!hasMoves($board))) {
  draw($board, $score);
  echo rtrim('Game Over'), PHP_EOL;
  break;
};
}
  draw($board, $score);
  if (has2048($board)) {
  echo rtrim('You win!'), PHP_EOL;
  break;
}
  if (!hasMoves($board)) {
  echo rtrim('Game Over'), PHP_EOL;
  break;
}
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
