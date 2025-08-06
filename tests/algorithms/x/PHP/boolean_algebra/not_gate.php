<?php
ini_set('memory_limit', '-1');
function not_gate($input) {
  if ($input == 0) {
  return 1;
}
  return 0;
}
echo rtrim(json_encode(not_gate(0), 1344)), PHP_EOL;
echo rtrim(json_encode(not_gate(1), 1344)), PHP_EOL;
