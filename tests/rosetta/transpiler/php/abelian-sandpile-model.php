<?php
$dim = 16;
function newPile($d) {
  global $dim, $handlePile, $drawPile, $main;
  $b = [];
  $y = 0;
  while ($y < $d) {
  $row = [];
  $x = 0;
  while ($x < $d) {
  $row = array_merge($row, [0]);
  $x = $x + 1;
};
  $b = array_merge($b, [$row]);
  $y = $y + 1;
};
  return $b;
}
function handlePile($pile, $x, $y) {
  global $dim, $newPile, $drawPile, $main;
  if ($pile[$y][$x] >= 4) {
  $pile[$y][$x] = $pile[$y][$x] - 4;
  if ($y > 0) {
  $pile[$y - 1][$x] = $pile[$y - 1][$x] + 1;
  if ($pile[$y - 1][$x] >= 4) {
  $pile = handlePile($pile, $x, $y - 1);
};
};
  if ($x > 0) {
  $pile[$y][$x - 1] = $pile[$y][$x - 1] + 1;
  if ($pile[$y][$x - 1] >= 4) {
  $pile = handlePile($pile, $x - 1, $y);
};
};
  if ($y < $dim - 1) {
  $pile[$y + 1][$x] = $pile[$y + 1][$x] + 1;
  if ($pile[$y + 1][$x] >= 4) {
  $pile = handlePile($pile, $x, $y + 1);
};
};
  if ($x < $dim - 1) {
  $pile[$y][$x + 1] = $pile[$y][$x + 1] + 1;
  if ($pile[$y][$x + 1] >= 4) {
  $pile = handlePile($pile, $x + 1, $y);
};
};
  $pile = handlePile($pile, $x, $y);
}
  return $pile;
}
function drawPile($pile, $d) {
  global $dim, $newPile, $handlePile, $main;
  $chars = [" ", "░", "▓", "█"];
  $row = 0;
  while ($row < $d) {
  $line = "";
  $col = 0;
  while ($col < $d) {
  $v = $pile[$row][$col];
  if ($v > 3) {
  $v = 3;
}
  $line = $line . $chars[$v];
  $col = $col + 1;
};
  echo $line, PHP_EOL;
  $row = $row + 1;
};
}
function main() {
  global $dim, $newPile, $handlePile, $drawPile;
  $pile = newPile(16);
  $hdim = 7;
  $pile[$hdim][$hdim] = 16;
  $pile = handlePile($pile, $hdim, $hdim);
  drawPile($pile, 16);
}
main();
