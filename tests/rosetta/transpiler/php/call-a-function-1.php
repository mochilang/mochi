<?php
ini_set('memory_limit', '-1');
function f() {
  global $g, $h;
  return [0, 0.0];
}
function g($a, $b) {
  global $f, $h;
  return 0;
}
function h($s, $nums) {
  global $f, $g;
}
