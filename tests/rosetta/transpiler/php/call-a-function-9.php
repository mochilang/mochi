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
  $ab = f();
  $a = $ab[0];
  $b = $ab[1];
  $cb = f()[1];
  $d = g($a, $cb);
  $e = g($d, $b);
  $i = g($d, 2.0);
  $list = [];
  $list = array_merge($list, [$a]);
  $list = array_merge($list, [$d]);
  $list = array_merge($list, [$e]);
  $list = array_merge($list, [$i]);
  $i = count($list);
}
main();
