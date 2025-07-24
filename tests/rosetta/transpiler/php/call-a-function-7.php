<?php
ini_set('memory_limit', '-1');
function f() {
  global $g, $h, $main;
  return [0, 0.0];
}
function g($a, $b) {
  global $f, $h, $main;
  return 0;
}
function h($s, $nums) {
  global $f, $g, $main;
}
function main() {
  global $f, $g, $h;
  if (2 * g(1, 3.0) + 4 > 0) {
}
}
main();
