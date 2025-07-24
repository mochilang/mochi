<?php
ini_set('memory_limit', '-1');
function push($h, $it) {
  global $step, $positions, $pad, $main;
  $h = array_merge($h, [$it]);
  $i = count($h) - 1;
  while ($i > 0 && $h[$i - 1]['s'] > $h[$i]['s']) {
  $tmp = $h[$i - 1];
  $h[$i - 1] = $h[$i];
  $h[$i] = $tmp;
  $i = $i - 1;
};
  return $h;
}
function step($h, $nv, $dir) {
  global $push, $positions, $pad, $main;
  while (count($h) == 0 || $nv * $nv <= $h[0]['s']) {
  $h = push($h, ['s' => $nv * $nv, 'a' => $nv, 'b' => 0]);
  $nv = $nv + 1;
};
  $s = $h[0]['s'];
  $v = [];
  while (count($h) > 0 && $h[0]['s'] == $s) {
  $it = $h[0];
  $h = array_slice($h, 1);
  $v = array_merge($v, [[$it['a'], $it['b']]]);
  if ($it['a'] > $it['b']) {
  $h = push($h, ['s' => $it['a'] * $it['a'] + ($it['b'] + 1) * ($it['b'] + 1), 'a' => $it['a'], 'b' => $it['b'] + 1]);
}
};
  $list = [];
  foreach ($v as $p) {
  $list = array_merge($list, [$p]);
};
  $temp = $list;
  foreach ($temp as $p) {
  if ($p[0] != $p[1]) {
  $list = array_merge($list, [[$p[1], $p[0]]]);
}
};
  $temp = $list;
  foreach ($temp as $p) {
  if ($p[1] != 0) {
  $list = array_merge($list, [[$p[0], -$p[1]]]);
}
};
  $temp = $list;
  foreach ($temp as $p) {
  if ($p[0] != 0) {
  $list = array_merge($list, [[-$p[0], $p[1]]]);
}
};
  $bestDot = -999999999;
  $best = $dir;
  foreach ($list as $p) {
  $cross = $p[0] * $dir[1] - $p[1] * $dir[0];
  if ($cross >= 0) {
  $dot = $p[0] * $dir[0] + $p[1] * $dir[1];
  if ($dot > $bestDot) {
  $bestDot = $dot;
  $best = $p;
};
}
};
  return ['d' => $best, 'heap' => $h, 'n' => $nv];
}
function positions($n) {
  global $push, $step, $pad, $main;
  $pos = [];
  $x = 0;
  $y = 0;
  $dir = [0, 1];
  $heap = [];
  $nv = 1;
  $i = 0;
  while ($i < $n) {
  $pos = array_merge($pos, [[$x, $y]]);
  $st = step($heap, $nv, $dir);
  $dir = $st['d'];
  $heap = $st['heap'];
  $nv = ord($st['n']);
  $x = $x + $dir[0];
  $y = $y + $dir[1];
  $i = $i + 1;
};
  return $pos;
}
function pad($s, $w) {
  global $push, $step, $positions, $main;
  $r = $s;
  while (strlen($r) < $w) {
  $r = $r . ' ';
};
  return $r;
}
function main() {
  global $push, $step, $positions, $pad;
  $pts = positions(40);
  echo rtrim('The first 40 Babylonian spiral points are:'), PHP_EOL;
  $line = '';
  $i = 0;
  while ($i < count($pts)) {
  $p = $pts[$i];
  $s = pad('(' . json_encode($p[0], 1344) . ', ' . json_encode($p[1], 1344) . ')', 10);
  $line = $line . $s;
  if (($i + 1) % 10 == 0) {
  echo rtrim($line), PHP_EOL;
  $line = '';
}
  $i = $i + 1;
};
}
main();
