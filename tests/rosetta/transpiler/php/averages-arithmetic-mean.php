<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
function mean($v) {
  global $main;
  if (count($v) == 0) {
  return ['ok' => false];
}
  $sum = 0.0;
  $i = 0;
  while ($i < count($v)) {
  $sum = $sum + $v[$i];
  $i = $i + 1;
};
  return ['ok' => true, 'mean' => $sum / (count($v))];
}
function main() {
  global $mean;
  $sets = [[], [3.0, 1.0, 4.0, 1.0, 5.0, 9.0], [100000000000000000000.0, 3.0, 1.0, 4.0, 1.0, 5.0, 9.0, -100000000000000000000.0], [10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.11], [10.0, 20.0, 30.0, 40.0, 50.0, -100.0, 4.7, -1100.0]];
  foreach ($sets as $v) {
  echo rtrim('Vector: ' . json_encode($v, 1344)), PHP_EOL;
  $r = mean($v);
  if ($r['ok']) {
  echo rtrim('Mean of ' . json_encode(_len($v), 1344) . ' numbers is ' . json_encode($r['mean'], 1344)), PHP_EOL;
} else {
  echo rtrim('Mean undefined'), PHP_EOL;
}
  echo rtrim(''), PHP_EOL;
};
}
main();
