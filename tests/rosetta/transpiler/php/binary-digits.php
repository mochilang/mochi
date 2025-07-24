<?php
ini_set('memory_limit', '-1');
function toBin($n) {
  if ($n == 0) {
  return '0';
}
  $bits = '';
  $x = $n;
  while ($x > 0) {
  $bits = json_encode($x % 2, 1344) . $bits;
  $x = intval((intdiv($x, 2)));
};
  return $bits;
}
for ($i = 0; $i < 16; $i++) {
  echo rtrim(toBin($i)), PHP_EOL;
}
