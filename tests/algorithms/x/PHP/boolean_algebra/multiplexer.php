<?php
ini_set('memory_limit', '-1');
function mux($input0, $input1, $select) {
  if (($input0 != 0 && $input0 != 1) || ($input1 != 0 && $input1 != 1) || ($select != 0 && $select != 1)) {
  $panic('Inputs and select signal must be 0 or 1');
}
  if ($select == 1) {
  return $input1;
}
  return $input0;
}
echo rtrim(json_encode(mux(0, 1, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(mux(0, 1, 1), 1344)), PHP_EOL;
echo rtrim(json_encode(mux(1, 0, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(mux(1, 0, 1), 1344)), PHP_EOL;
