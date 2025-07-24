<?php
ini_set('memory_limit', '-1');
function fetchSomething() {
  return 0;
}
function doPos($x) {
}
function doNeg($x) {
}
function example4() {
  $x = fetchSomething();
  if ($x > 0) {
  doPos($x);
} else {
  doNeg($x);
}
}
