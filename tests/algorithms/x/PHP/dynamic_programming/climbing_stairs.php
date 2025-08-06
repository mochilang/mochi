<?php
ini_set('memory_limit', '-1');
function climb_stairs($number_of_steps) {
  if ($number_of_steps <= 0) {
  $panic('number_of_steps needs to be positive');
}
  if ($number_of_steps == 1) {
  return 1;
}
  $previous = 1;
  $current = 1;
  $i = 0;
  while ($i < $number_of_steps - 1) {
  $next = $current + $previous;
  $previous = $current;
  $current = $next;
  $i = $i + 1;
};
  return $current;
}
echo rtrim(json_encode(climb_stairs(3), 1344)), PHP_EOL;
echo rtrim(json_encode(climb_stairs(1), 1344)), PHP_EOL;
