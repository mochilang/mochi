<?php
ini_set('memory_limit', '-1');
function sqrtApprox($x) {
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function electrical_impedance($resistance, $reactance, $impedance) {
  $zero_count = 0;
  if ($resistance == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($reactance == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($impedance == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($zero_count != 1) {
  $panic('One and only one argument must be 0');
}
  if ($resistance == 0.0) {
  $value = sqrtApprox($impedance * $impedance - $reactance * $reactance);
  return ['resistance' => $value];
} else {
  if ($reactance == 0.0) {
  $value = sqrtApprox($impedance * $impedance - $resistance * $resistance);
  return ['reactance' => $value];
} else {
  if ($impedance == 0.0) {
  $value = sqrtApprox($resistance * $resistance + $reactance * $reactance);
  return ['impedance' => $value];
} else {
  $panic('Exactly one argument must be 0');
};
};
}
}
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(electrical_impedance(3.0, 4.0, 0.0), 1344)))))), PHP_EOL;
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(electrical_impedance(0.0, 4.0, 5.0), 1344)))))), PHP_EOL;
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(electrical_impedance(3.0, 0.0, 5.0), 1344)))))), PHP_EOL;
