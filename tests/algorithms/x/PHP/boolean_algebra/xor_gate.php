<?php
ini_set('memory_limit', '-1');
function xor_gate($input_1, $input_2) {
  $zeros = 0;
  if ($input_1 == 0) {
  $zeros = $zeros + 1;
}
  if ($input_2 == 0) {
  $zeros = $zeros + 1;
}
  return $zeros % 2;
}
echo rtrim(json_encode(xor_gate(0, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(xor_gate(0, 1), 1344)), PHP_EOL;
echo rtrim(json_encode(xor_gate(1, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(xor_gate(1, 1), 1344)), PHP_EOL;
