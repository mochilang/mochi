<?php
ini_set('memory_limit', '-1');
function ohms_law($voltage, $current, $resistance) {
  $zeros = 0;
  if ($voltage == 0.0) {
  $zeros = $zeros + 1;
}
  if ($current == 0.0) {
  $zeros = $zeros + 1;
}
  if ($resistance == 0.0) {
  $zeros = $zeros + 1;
}
  if ($zeros != 1) {
  echo rtrim('One and only one argument must be 0'), PHP_EOL;
  return [];
}
  if ($resistance < 0.0) {
  echo rtrim('Resistance cannot be negative'), PHP_EOL;
  return [];
}
  if ($voltage == 0.0) {
  return ['voltage' => $current * $resistance];
}
  if ($current == 0.0) {
  return ['current' => $voltage / $resistance];
}
  return ['resistance' => $voltage / $current];
}
echo str_replace('    ', '  ', json_encode(ohms_law(10.0, 0.0, 5.0), 128)), PHP_EOL;
echo str_replace('    ', '  ', json_encode(ohms_law(-10.0, 1.0, 0.0), 128)), PHP_EOL;
echo str_replace('    ', '  ', json_encode(ohms_law(0.0, -1.5, 2.0), 128)), PHP_EOL;
