<?php
function angleDiff($b1, $b2) {
  global $testCases;
  $d = $b2 - $b1;
  if ($d < 0 - 180.0) {
  return $d + 360.0;
}
  if ($d > 180.0) {
  return $d - 360.0;
}
  return $d;
}
$testCases = [[20.0, 45.0], [0 - 45.0, 45.0], [0 - 85.0, 90.0], [0 - 95.0, 90.0], [0 - 45.0, 125.0], [0 - 45.0, 145.0], [29.4803, 0 - 88.6381], [0 - 78.3251, 0 - 159.036]];
foreach ($testCases as $tc) {
  echo json_encode(angleDiff($tc[0], $tc[1]), 1344), PHP_EOL;
}
