<?php
ini_set('memory_limit', '-1');
function nand_gate($a, $b) {
  if ($a != 0 && $b != 0) {
  return 0;
}
  return 1;
}
echo rtrim(json_encode(nand_gate(0, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(nand_gate(0, 1), 1344)), PHP_EOL;
echo rtrim(json_encode(nand_gate(1, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(nand_gate(1, 1), 1344)), PHP_EOL;
