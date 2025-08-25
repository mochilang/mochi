<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function hexagonal($n) {
  global $samples;
  if ($n < 1) {
  _panic('Input must be a positive integer');
}
  return $n * (2 * $n - 1);
}
$samples = [4, 11, 22];
foreach ($samples as $s) {
  echo rtrim(json_encode(hexagonal($s), 1344)), PHP_EOL;
}
