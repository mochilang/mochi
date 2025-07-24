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
  h('ex1', []);
  h('ex2', [1, 2]);
  h('ex3', [1, 2, 3, 4]);
  $list = [1, 2, 3, 4];
  h('ex4', $list);
}
main();
