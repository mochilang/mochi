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
  f();
  g(1, 2.0);
  $res = f();
  g($res[0], $res[1]);
  g(g(1, 2.0), 3.0);
}
main();
