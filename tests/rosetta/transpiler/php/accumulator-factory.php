<?php
ini_set('memory_limit','-1');
function accumulator($sum) {
  global $main;
  $store = [$sum];
  $add = function($nv) use ($sum, $store) {
  $store[0] = $store[0] + $nv;
  return $store[0];
};
  return $add;
}
function main() {
  global $accumulator;
  $x = accumulator(1);
  $x(5);
  accumulator(3);
  echo json_encode($x(2.3), 1344), PHP_EOL;
}
main();
