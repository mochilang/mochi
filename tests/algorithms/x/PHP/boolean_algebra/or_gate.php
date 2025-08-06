<?php
ini_set('memory_limit', '-1');
function or_gate($input_1, $input_2) {
  if (in_array(1, [$input_1, $input_2])) {
  return 1;
}
  return 0;
}
echo rtrim(json_encode(or_gate(0, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(or_gate(0, 1), 1344)), PHP_EOL;
echo rtrim(json_encode(or_gate(1, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(or_gate(1, 1), 1344)), PHP_EOL;
