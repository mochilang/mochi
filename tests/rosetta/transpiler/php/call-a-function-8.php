<?php
ini_set('memory_limit', '-1');
function mapString($s, $f) {
  global $main;
  $out = '';
  $i = 0;
  while ($i < strlen($s)) {
  $out = $out . $f(substr($s, $i, $i + 1 - $i));
  $i = $i + 1;
};
  return $out;
}
function main() {
  global $mapString;
  $fn = function($r) use ($fn, &$mapString) {
  return ($r == ' ' ? '' : $r);
};
  mapString('Spaces removed', $fn);
  mapString('Test', function($r) use ($fn, &$mapString) {
  return strtolower($r);
});
  mapString('shift', function($r) use ($fn, &$mapString) {
  return $r;
});
}
main();
