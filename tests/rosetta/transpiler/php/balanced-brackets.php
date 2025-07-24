<?php
ini_set('memory_limit', '-1');
$seed = 1;
function prng($max) {
  global $seed, $gen, $testBalanced, $main;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed % $max;
}
function gen($n) {
  global $seed, $prng, $testBalanced, $main;
  $arr = [];
  $i = 0;
  while ($i < $n) {
  $arr = array_merge($arr, ['[']);
  $arr = array_merge($arr, [']']);
  $i = $i + 1;
};
  $j = count($arr) - 1;
  while ($j > 0) {
  $k = prng($j + 1);
  $tmp = $arr[$j];
  $arr[$j] = $arr[$k];
  $arr[$k] = $tmp;
  $j = $j - 1;
};
  $out = '';
  foreach ($arr as $ch) {
  $out = $out . $ch;
};
  return $out;
}
function testBalanced($s) {
  global $seed, $prng, $gen, $main;
  $open = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c == '[') {
  $open = $open + 1;
} else {
  if ($c == ']') {
  if ($open == 0) {
  echo rtrim($s . ': not ok'), PHP_EOL;
  return;
};
  $open = $open - 1;
} else {
  echo rtrim($s . ': not ok'), PHP_EOL;
  return;
};
}
  $i = $i + 1;
};
  if ($open == 0) {
  echo rtrim($s . ': ok'), PHP_EOL;
} else {
  echo rtrim($s . ': not ok'), PHP_EOL;
}
}
function main() {
  global $seed, $prng, $gen, $testBalanced;
  $i = 0;
  while ($i < 10) {
  testBalanced(gen($i));
  $i = $i + 1;
};
  testBalanced('()');
}
main();
