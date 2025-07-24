<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
function randDigit() {
  global $main;
  return (_now() % 9) + 1;
}
function main() {
  global $randDigit;
  $digits = [];
  for ($i = 0; $i < 4; $i++) {
  $digits = array_merge($digits, [randDigit()]);
};
  $numstr = '';
  for ($i = 0; $i < 4; $i++) {
  $numstr = $numstr . json_encode($digits[$i], 1344);
};
  echo 'Your numbers: ' . $numstr . '
', PHP_EOL;
  echo 'Enter RPN: ', PHP_EOL;
  $expr = trim(fgets(STDIN));
  if (strlen($expr) != 7) {
  echo 'invalid. expression length must be 7. (4 numbers, 3 operators, no spaces)', PHP_EOL;
  return;
}
  $stack = [];
  $i = 0;
  $valid = true;
  while ($i < strlen($expr)) {
  $ch = substr($expr, $i, $i + 1 - $i);
  if ($ch >= '0' && $ch <= '9') {
  if (count($digits) == 0) {
  echo 'too many numbers.', PHP_EOL;
  return;
};
  $j = 0;
  while ($digits[$j] != intval($ch) - intval('0')) {
  $j = $j + 1;
  if ($j == count($digits)) {
  echo 'wrong numbers.', PHP_EOL;
  return;
}
};
  $digits = array_merge(array_slice($digits, 0, $j - 0), array_slice($digits, $j + 1));
  $stack = array_merge($stack, [float(intval($ch) - intval('0'))]);
} else {
  if (count($stack) < 2) {
  echo 'invalid expression syntax.', PHP_EOL;
  $valid = false;
  break;
};
  $b = $stack[count($stack) - 1];
  $a = $stack[count($stack) - 2];
  if ($ch == '+') {
  $stack[count($stack) - 2] = $a + $b;
} else {
  if ($ch == '-') {
  $stack[count($stack) - 2] = $a - $b;
} else {
  if ($ch == '*') {
  $stack[count($stack) - 2] = $a * $b;
} else {
  if ($ch == '/') {
  $stack[count($stack) - 2] = $a / $b;
} else {
  echo $ch . ' invalid.', PHP_EOL;
  $valid = false;
  break;
};
};
};
};
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
}
  $i = $i + 1;
};
  if ($valid) {
  if (abs($stack[0] - 24.0) > 0.000001) {
  echo 'incorrect. ' . json_encode($stack[0], 1344) . ' != 24', PHP_EOL;
} else {
  echo 'correct.', PHP_EOL;
};
}
}
main();
