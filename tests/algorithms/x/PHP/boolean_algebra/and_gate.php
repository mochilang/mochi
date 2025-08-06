<?php
ini_set('memory_limit', '-1');
function and_gate($input_1, $input_2) {
  if ($input_1 != 0 && $input_2 != 0) {
  return 1;
}
  return 0;
}
function n_input_and_gate($inputs) {
  $i = 0;
  while ($i < count($inputs)) {
  if ($inputs[$i] == 0) {
  return 0;
}
  $i = $i + 1;
};
  return 1;
}
echo rtrim(json_encode(and_gate(0, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(and_gate(0, 1), 1344)), PHP_EOL;
echo rtrim(json_encode(and_gate(1, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(and_gate(1, 1), 1344)), PHP_EOL;
echo rtrim(json_encode(n_input_and_gate([1, 0, 1, 1, 0]), 1344)), PHP_EOL;
echo rtrim(json_encode(n_input_and_gate([1, 1, 1, 1, 1]), 1344)), PHP_EOL;
