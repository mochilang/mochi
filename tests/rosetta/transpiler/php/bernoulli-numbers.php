<?php
ini_set('memory_limit', '-1');
function bernoulli($n) {
  global $padStart, $b, $numStr, $denStr;
  $a = [];
  $m = 0;
  while ($m <= $n) {
  $a = array_merge($a, [intdiv(1, (($m + 1)))]);
  $j = $m;
  while ($j >= 1) {
  $a[$j - 1] = ($j) * ($a[$j - 1] - $a[$j]);
  $j = $j - 1;
};
  $m = $m + 1;
};
  return $a[0];
}
function padStart($s, $width, $pad) {
  global $bernoulli, $b, $numStr, $denStr;
  $out = $s;
  while (strlen($out) < $width) {
  $out = $pad . $out;
};
  return $out;
}
for ($i = 0; $i < 61; $i++) {
  $b = bernoulli($i);
  if (num($b) != 0) {
  $numStr = json_encode(num($b), 1344);
  $denStr = json_encode(denom($b), 1344);
  echo rtrim('B(' . str_pad(json_encode($i, 1344), 2, ' ', STR_PAD_LEFT) . ') =' . str_pad($numStr, 45, ' ', STR_PAD_LEFT) . '/' . $denStr), PHP_EOL;
}
}
