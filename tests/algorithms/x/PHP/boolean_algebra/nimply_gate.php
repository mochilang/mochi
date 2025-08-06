<?php
ini_set('memory_limit', '-1');
function nimply_gate($a, $b) {
  if ($a == 1 && $b == 0) {
  return 1;
}
  return 0;
}
echo rtrim(json_encode(nimply_gate(0, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(nimply_gate(0, 1), 1344)), PHP_EOL;
echo rtrim(json_encode(nimply_gate(1, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(nimply_gate(1, 1), 1344)), PHP_EOL;
