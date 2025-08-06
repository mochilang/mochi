<?php
ini_set('memory_limit', '-1');
function imply_gate($input_1, $input_2) {
  if ($input_1 == 0 || $input_2 == 1) {
  return 1;
}
  return 0;
}
echo rtrim(json_encode(imply_gate(0, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(imply_gate(0, 1), 1344)), PHP_EOL;
echo rtrim(json_encode(imply_gate(1, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(imply_gate(1, 1), 1344)), PHP_EOL;
