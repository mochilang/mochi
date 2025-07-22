<?php
$board = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0];
$solved = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0];
$empty = 15;
$moves = 0;
$quit = false;
function randMove() {
  global $board, $solved, $empty, $moves, $quit, $isSolved, $isValidMove, $doMove, $mochi_shuffle, $printBoard, $playOneMove, $play, $main;
  return hrtime(true) % 4;
}
function isSolved() {
  global $board, $solved, $empty, $moves, $quit, $randMove, $isValidMove, $doMove, $mochi_shuffle, $printBoard, $playOneMove, $play, $main;
  $i = 0;
  while ($i < 16) {
  if ($board[$i] != $solved[$i]) {
  return false;
}
  $i = $i + 1;
};
  return true;
}
function isValidMove($m) {
  global $board, $solved, $empty, $moves, $quit, $randMove, $isSolved, $doMove, $mochi_shuffle, $printBoard, $playOneMove, $play, $main;
  if ($m == 0) {
  return ["idx" => $empty - 4, "ok" => $empty / 4 > 0];
}
  if ($m == 1) {
  return ["idx" => $empty + 4, "ok" => $empty / 4 < 3];
}
  if ($m == 2) {
  return ["idx" => $empty + 1, "ok" => $empty % 4 < 3];
}
  if ($m == 3) {
  return ["idx" => $empty - 1, "ok" => $empty % 4 > 0];
}
  return ["idx" => 0, "ok" => false];
}
function doMove($m) {
  global $board, $solved, $empty, $moves, $quit, $randMove, $isSolved, $isValidMove, $mochi_shuffle, $printBoard, $playOneMove, $play, $main;
  $r = isValidMove($m);
  if (!$r["ok"]) {
  return false;
}
  $i = $empty;
  $j = intval($r["idx"]);
  $tmp = $board[$i];
  $board[$i] = $board[$j];
  $board[$j] = $tmp;
  $empty = $j;
  $moves = $moves + 1;
  return true;
}
function mochi_shuffle($n) {
  global $board, $solved, $empty, $moves, $quit, $randMove, $isSolved, $isValidMove, $doMove, $printBoard, $playOneMove, $play, $main;
  $i = 0;
  while ($i < $n || isSolved()) {
  if (doMove(randMove())) {
  $i = $i + 1;
}
};
}
function printBoard() {
  global $board, $solved, $empty, $moves, $quit, $randMove, $isSolved, $isValidMove, $doMove, $mochi_shuffle, $playOneMove, $play, $main;
  $line = "";
  $i = 0;
  while ($i < 16) {
  $val = $board[$i];
  if ($val == 0) {
  $line = $line . "  .";
} else {
  $s = strval($val);
  if ($val < 10) {
  $line = $line . "  " . $s;
} else {
  $line = $line . " " . $s;
};
}
  if ($i % 4 == 3) {
  echo $line, PHP_EOL;
  $line = "";
}
  $i = $i + 1;
};
}
function playOneMove() {
  global $board, $solved, $empty, $moves, $quit, $randMove, $isSolved, $isValidMove, $doMove, $mochi_shuffle, $printBoard, $play, $main;
  while (true) {
  echo "Enter move #" . strval($moves + 1) . " (U, D, L, R, or Q): ", PHP_EOL;
  $s = trim(fgets(STDIN));
  if ($s == "") {
  continue;
}
  $c = substr($s, 0, 1 - 0);
  $m = 0;
  if ($c == "U" || $c == "u") {
  $m = 0;
} else {
  if ($c == "D" || $c == "d") {
  $m = 1;
} else {
  if ($c == "R" || $c == "r") {
  $m = 2;
} else {
  if ($c == "L" || $c == "l") {
  $m = 3;
} else {
  if ($c == "Q" || $c == "q") {
  echo "Quiting after " . strval($moves) . " moves.", PHP_EOL;
  $quit = true;
  return;
} else {
  echo "Please enter \"U\", \"D\", \"L\", or \"R\" to move the empty cell\n" . "up, down, left, or right. You can also enter \"Q\" to quit.\n" . "Upper or lowercase is accepted and only the first non-blank\n" . "character is important (i.e. you may enter \"up\" if you like).", PHP_EOL;
  continue;
};
};
};
};
}
  if (!doMove($m)) {
  echo "That is not a valid move at the moment.", PHP_EOL;
  continue;
}
  return;
};
}
function play() {
  global $board, $solved, $empty, $moves, $quit, $randMove, $isSolved, $isValidMove, $doMove, $mochi_shuffle, $printBoard, $playOneMove, $main;
  echo "Starting board:", PHP_EOL;
  while (!$quit && isSolved() == false) {
  echo "", PHP_EOL;
  printBoard();
  playOneMove();
};
  if (isSolved()) {
  echo "You solved the puzzle in " . strval($moves) . " moves.", PHP_EOL;
}
}
function main() {
  global $board, $solved, $empty, $moves, $quit, $randMove, $isSolved, $isValidMove, $doMove, $mochi_shuffle, $printBoard, $playOneMove, $play;
  mochi_shuffle(50);
  play();
}
main();
