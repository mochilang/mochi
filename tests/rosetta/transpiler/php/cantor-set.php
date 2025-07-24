<?php
ini_set('memory_limit', '-1');
$width = 81;
$height = 5;
$lines = [];
for ($i = 0; $i < $height; $i++) {
  $row = '';
  $j = 0;
  while ($j < $width) {
  $row = $row . '*';
  $j = $j + 1;
};
  $lines = array_merge($lines, [$row]);
}
function setChar($s, $idx, $ch) {
  global $width, $height, $lines, $row, $j, $stack, $frame, $start, $lenSeg, $index, $seg, $i;
  return substr($s, 0, $idx - 0) . $ch . substr($s, $idx + 1, strlen($s) - $idx + 1);
}
$stack = [['start' => 0, 'len' => $width, 'index' => 1]];
while (count($stack) > 0) {
  $frame = $stack[count($stack) - 1];
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
  $start = $frame['start'];
  $lenSeg = $frame['len'];
  $index = $frame['index'];
  $seg = intval((intdiv($lenSeg, 3)));
  if ($seg == 0) {
  continue;
}
  $i = $index;
  while ($i < $height) {
  $j = $start + $seg;
  while ($j < $start + 2 * $seg) {
  $lines[$i] = setChar($lines[$i], $j, ' ');
  $j = $j + 1;
};
  $i = $i + 1;
};
  $stack = array_merge($stack, [['start' => $start, 'len' => $seg, 'index' => $index + 1]]);
  $stack = array_merge($stack, [['start' => $start + $seg * 2, 'len' => $seg, 'index' => $index + 1]]);
}
foreach ($lines as $line) {
  echo rtrim($line), PHP_EOL;
}
