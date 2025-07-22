<?php
$SIZE = 4;
function newBoard() {
  global $SIZE, $spawnTile, $pad, $draw, $reverseRow, $slideLeft, $moveLeft, $moveRight, $getCol, $setCol, $moveUp, $moveDown, $hasMoves, $has2048, $board, $r, $full, $score, $cmd, $moved, $m, $m, $m, $m, $r2;
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
  return $b;
}
function spawnTile($b) {
  global $SIZE, $newBoard, $pad, $draw, $reverseRow, $slideLeft, $moveLeft, $moveRight, $getCol, $setCol, $moveUp, $moveDown, $hasMoves, $has2048, $board, $r, $full, $score, $cmd, $moved, $m, $m, $m, $m, $r2;
  $empty = [];
  $y = 0;
  while ($y < $SIZE) {
  $x = 0;
  while ($x < $SIZE) {
  if ($b[$y][$x] == 0) {
  $empty = array_merge($empty, [[$x, $y]]);
}
  $x = $x + 1;
};
  $y = $y + 1;
};
  if (count($empty) == 0) {
  return ["board" => $b, "full" => true];
}
  $idx = hrtime(true) % count($empty);
  $cell = $empty[$idx];
  $val = 4;
  if (hrtime(true) % 10 < 9) {
  $val = 2;
}
  $b[$cell[1]][$cell[0]] = $val;
  return ["board" => $b, "full" => count($empty) == 1];
}
function pad($n) {
  global $SIZE, $newBoard, $spawnTile, $draw, $reverseRow, $slideLeft, $moveLeft, $moveRight, $getCol, $setCol, $moveUp, $moveDown, $hasMoves, $has2048, $board, $r, $full, $score, $cmd, $moved, $m, $m, $m, $m, $r2;
  $s = strval($n);
  $pad = 4 - strlen($s);
  $i = 0;
  $out = "";
  while ($i < $pad) {
  $out = $out . " ";
  $i = $i + 1;
};
  return $out . $s;
}
function draw($b, $score) {
  global $SIZE, $newBoard, $spawnTile, $pad, $reverseRow, $slideLeft, $moveLeft, $moveRight, $getCol, $setCol, $moveUp, $moveDown, $hasMoves, $has2048, $board, $r, $full, $cmd, $moved, $m, $m, $m, $m, $r2;
  echo "Score: " . strval($score), PHP_EOL;
  $y = 0;
  while ($y < $SIZE) {
  echo "+----+----+----+----+", PHP_EOL;
  $line = "|";
  $x = 0;
  while ($x < $SIZE) {
  $v = $b[$y][$x];
  if ($v == 0) {
  $line = $line . "    |";
} else {
  $line = $line . pad($v) . "|";
}
  $x = $x + 1;
};
  echo $line, PHP_EOL;
  $y = $y + 1;
};
  echo "+----+----+----+----+", PHP_EOL;
  echo "W=Up S=Down A=Left D=Right Q=Quit", PHP_EOL;
}
function reverseRow($r) {
  global $SIZE, $newBoard, $spawnTile, $pad, $draw, $slideLeft, $moveLeft, $moveRight, $getCol, $setCol, $moveUp, $moveDown, $hasMoves, $has2048, $board, $full, $score, $cmd, $moved, $m, $m, $m, $m, $r2;
  $out = [];
  $i = count($r) - 1;
  while ($i >= 0) {
  $out = array_merge($out, [$r[$i]]);
  $i = $i - 1;
};
  return $out;
}
function slideLeft($row) {
  global $SIZE, $newBoard, $spawnTile, $pad, $draw, $reverseRow, $moveLeft, $moveRight, $getCol, $setCol, $moveUp, $moveDown, $hasMoves, $has2048, $board, $r, $full, $score, $cmd, $moved, $m, $m, $m, $m, $r2;
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
  return ["row" => $res, "gain" => $gain];
}
function moveLeft($b, $score) {
  global $SIZE, $newBoard, $spawnTile, $pad, $draw, $reverseRow, $slideLeft, $moveRight, $getCol, $setCol, $moveUp, $moveDown, $hasMoves, $has2048, $board, $r, $full, $cmd, $moved, $m, $m, $m, $m, $r2;
  $moved = false;
  $y = 0;
  while ($y < $SIZE) {
  $r = slideLeft($b[$y]);
  $new = $r["row"];
  $score = $score + $r["gain"];
  $x = 0;
  while ($x < $SIZE) {
  if ($b[$y][$x] != $new[$x]) {
  $moved = true;
}
  $b[$y][$x] = $new[$x];
  $x = $x + 1;
};
  $y = $y + 1;
};
  return ["board" => $b, "score" => $score, "moved" => $moved];
}
function moveRight($b, $score) {
  global $SIZE, $newBoard, $spawnTile, $pad, $draw, $reverseRow, $slideLeft, $moveLeft, $getCol, $setCol, $moveUp, $moveDown, $hasMoves, $has2048, $board, $r, $full, $cmd, $moved, $m, $m, $m, $m, $r2;
  $moved = false;
  $y = 0;
  while ($y < $SIZE) {
  $rev = reverseRow($b[$y]);
  $r = slideLeft($rev);
  $rev = $r["row"];
  $score = $score + $r["gain"];
  $rev = reverseRow($rev);
  $x = 0;
  while ($x < $SIZE) {
  if ($b[$y][$x] != $rev[$x]) {
  $moved = true;
}
  $b[$y][$x] = $rev[$x];
  $x = $x + 1;
};
  $y = $y + 1;
};
  return ["board" => $b, "score" => $score, "moved" => $moved];
}
function getCol($b, $x) {
  global $SIZE, $newBoard, $spawnTile, $pad, $draw, $reverseRow, $slideLeft, $moveLeft, $moveRight, $setCol, $moveUp, $moveDown, $hasMoves, $has2048, $board, $r, $full, $score, $cmd, $moved, $m, $m, $m, $m, $r2;
  $col = [];
  $y = 0;
  while ($y < $SIZE) {
  $col = array_merge($col, [$b[$y][$x]]);
  $y = $y + 1;
};
  return $col;
}
function setCol($b, $x, $col) {
  global $SIZE, $newBoard, $spawnTile, $pad, $draw, $reverseRow, $slideLeft, $moveLeft, $moveRight, $getCol, $moveUp, $moveDown, $hasMoves, $has2048, $board, $r, $full, $score, $cmd, $moved, $m, $m, $m, $m, $r2;
  $y = 0;
  while ($y < $SIZE) {
  $b[$y][$x] = $col[$y];
  $y = $y + 1;
};
}
function moveUp($b, $score) {
  global $SIZE, $newBoard, $spawnTile, $pad, $draw, $reverseRow, $slideLeft, $moveLeft, $moveRight, $getCol, $setCol, $moveDown, $hasMoves, $has2048, $board, $r, $full, $cmd, $moved, $m, $m, $m, $m, $r2;
  $moved = false;
  $x = 0;
  while ($x < $SIZE) {
  $col = getCol($b, $x);
  $r = slideLeft($col);
  $new = $r["row"];
  $score = $score + $r["gain"];
  $y = 0;
  while ($y < $SIZE) {
  if ($b[$y][$x] != $new[$y]) {
  $moved = true;
}
  $b[$y][$x] = $new[$y];
  $y = $y + 1;
};
  $x = $x + 1;
};
  return ["board" => $b, "score" => $score, "moved" => $moved];
}
function moveDown($b, $score) {
  global $SIZE, $newBoard, $spawnTile, $pad, $draw, $reverseRow, $slideLeft, $moveLeft, $moveRight, $getCol, $setCol, $moveUp, $hasMoves, $has2048, $board, $r, $full, $cmd, $moved, $m, $m, $m, $m, $r2;
  $moved = false;
  $x = 0;
  while ($x < $SIZE) {
  $col = reverseRow(getCol($b, $x));
  $r = slideLeft($col);
  $col = $r["row"];
  $score = $score + $r["gain"];
  $col = reverseRow($col);
  $y = 0;
  while ($y < $SIZE) {
  if ($b[$y][$x] != $col[$y]) {
  $moved = true;
}
  $b[$y][$x] = $col[$y];
  $y = $y + 1;
};
  $x = $x + 1;
};
  return ["board" => $b, "score" => $score, "moved" => $moved];
}
function hasMoves($b) {
  global $SIZE, $newBoard, $spawnTile, $pad, $draw, $reverseRow, $slideLeft, $moveLeft, $moveRight, $getCol, $setCol, $moveUp, $moveDown, $has2048, $board, $r, $full, $score, $cmd, $moved, $m, $m, $m, $m, $r2;
  $y = 0;
  while ($y < $SIZE) {
  $x = 0;
  while ($x < $SIZE) {
  if ($b[$y][$x] == 0) {
  return true;
}
  if ($x + 1 < $SIZE && $b[$y][$x] == $b[$y][$x + 1]) {
  return true;
}
  if ($y + 1 < $SIZE && $b[$y][$x] == $b[$y + 1][$x]) {
  return true;
}
  $x = $x + 1;
};
  $y = $y + 1;
};
  return false;
}
function has2048($b) {
  global $SIZE, $newBoard, $spawnTile, $pad, $draw, $reverseRow, $slideLeft, $moveLeft, $moveRight, $getCol, $setCol, $moveUp, $moveDown, $hasMoves, $board, $r, $full, $score, $cmd, $moved, $m, $m, $m, $m, $r2;
  $y = 0;
  while ($y < $SIZE) {
  $x = 0;
  while ($x < $SIZE) {
  if ($b[$y][$x] >= 2048) {
  return true;
}
  $x = $x + 1;
};
  $y = $y + 1;
};
  return false;
}
$board = newBoard();
$r = spawnTile($board);
$board = $r["board"];
$full = $r["full"];
$r = spawnTile($board);
$board = $r["board"];
$full = $r["full"];
$score = 0;
draw($board, $score);
while (true) {
  echo "Move: ", PHP_EOL;
  $cmd = trim(fgets(STDIN));
  $moved = false;
  if ($cmd == "a" || $cmd == "A") {
  $m = moveLeft($board, $score);
  $board = $m["board"];
  $score = $m["score"];
  $moved = $m["moved"];
}
  if ($cmd == "d" || $cmd == "D") {
  $m = moveRight($board, $score);
  $board = $m["board"];
  $score = $m["score"];
  $moved = $m["moved"];
}
  if ($cmd == "w" || $cmd == "W") {
  $m = moveUp($board, $score);
  $board = $m["board"];
  $score = $m["score"];
  $moved = $m["moved"];
}
  if ($cmd == "s" || $cmd == "S") {
  $m = moveDown($board, $score);
  $board = $m["board"];
  $score = $m["score"];
  $moved = $m["moved"];
}
  if ($cmd == "q" || $cmd == "Q") {
  break;
}
  if ($moved) {
  $r2 = spawnTile($board);
  $board = $r2["board"];
  $full = $r2["full"];
  if ($full && (!hasMoves($board))) {
  draw($board, $score);
  echo "Game Over", PHP_EOL;
  break;
};
}
  draw($board, $score);
  if (has2048($board)) {
  echo "You win!", PHP_EOL;
  break;
}
  if (!hasMoves($board)) {
  echo "Game Over", PHP_EOL;
  break;
}
}
