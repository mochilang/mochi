<?php
ini_set('memory_limit', '-1');
function wheatstone_solver($resistance_1, $resistance_2, $resistance_3) {
  if ($resistance_1 <= 0.0 || $resistance_2 <= 0.0 || $resistance_3 <= 0.0) {
  $panic('All resistance values must be positive');
}
  return ($resistance_2 / $resistance_1) * $resistance_3;
}
echo rtrim(json_encode(wheatstone_solver(2.0, 4.0, 5.0), 1344)), PHP_EOL;
echo rtrim(json_encode(wheatstone_solver(356.0, 234.0, 976.0), 1344)), PHP_EOL;
