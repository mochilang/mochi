<?php
function each(&$xs, $f) {
  global $Map, $main;
  foreach ($xs as $x) {
  $f($x);
};
}
function Map(&$xs, $f) {
  global $each, $main;
  $r = [];
  foreach ($xs as $x) {
  $r = array_merge($r, [$f($x)]);
};
  return $r;
}
function main() {
  global $each, $Map;
  $s = [1, 2, 3, 4, 5];
  each($s, function($i) use ($s) {
  echo json_encode($i * $i, 1344), PHP_EOL;
});
  echo json_encode(Map($s, function($i) use ($s) {
  return $i * $i;
}), 1344), PHP_EOL;
}
main();
