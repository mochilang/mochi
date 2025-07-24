<?php
ini_set('memory_limit', '-1');
function main() {
  $list = [];
  $a = 1;
  $d = 2;
  $e = 3;
  $i = 4;
  $list = array_merge($list, [$a]);
  $list = array_merge($list, [$d]);
  $list = array_merge($list, [$e]);
  $list = array_merge($list, [$i]);
  $i = count($list);
}
main();
