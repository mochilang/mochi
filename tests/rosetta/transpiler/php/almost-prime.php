<?php
function kPrime($n, $k) {
  global $gen, $main;
  $nf = 0;
  $i = 2;
  while ($i <= $n) {
  while ($n % $i == 0) {
  if ($nf == $k) {
  return false;
}
  $nf = $nf + 1;
  $n = intdiv($n, $i);
};
  $i = $i + 1;
};
  return $nf == $k;
}
function gen($k, $count) {
  global $kPrime, $main;
  $r = [];
  $n = 2;
  while (count($r) < $count) {
  if (kPrime($n, $k)) {
  $r = array_merge($r, [$n]);
}
  $n = $n + 1;
};
  return $r;
}
function main() {
  global $kPrime, $gen;
  $k = 1;
  while ($k <= 5) {
  echo json_encode($k, 1344) . " " . json_encode(gen($k, 10), 1344), PHP_EOL;
  $k = $k + 1;
};
}
main();
