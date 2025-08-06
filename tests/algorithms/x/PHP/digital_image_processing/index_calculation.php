<?php
ini_set('memory_limit', '-1');
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function ndvi($red, $nir) {
  return ($nir - $red) / ($nir + $red);
}
function bndvi($blue, $nir) {
  return ($nir - $blue) / ($nir + $blue);
}
function gndvi($green, $nir) {
  return ($nir - $green) / ($nir + $green);
}
function ndre($redEdge, $nir) {
  return ($nir - $redEdge) / ($nir + $redEdge);
}
function ccci($red, $redEdge, $nir) {
  return ndre($redEdge, $nir) / ndvi($red, $nir);
}
function cvi($red, $green, $nir) {
  return ($nir * $red) / ($green * $green);
}
function gli($red, $green, $blue) {
  return (2.0 * $green - $red - $blue) / (2.0 * $green + $red + $blue);
}
function dvi($red, $nir) {
  return $nir / $red;
}
function calc($index, $red, $green, $blue, $redEdge, $nir) {
  if ($index == 'NDVI') {
  return ndvi($red, $nir);
}
  if ($index == 'BNDVI') {
  return bndvi($blue, $nir);
}
  if ($index == 'GNDVI') {
  return gndvi($green, $nir);
}
  if ($index == 'NDRE') {
  return ndre($redEdge, $nir);
}
  if ($index == 'CCCI') {
  return ccci($red, $redEdge, $nir);
}
  if ($index == 'CVI') {
  return cvi($red, $green, $nir);
}
  if ($index == 'GLI') {
  return gli($red, $green, $blue);
}
  if ($index == 'DVI') {
  return dvi($red, $nir);
}
  return 0.0;
}
function main() {
  $red = 50.0;
  $green = 30.0;
  $blue = 10.0;
  $redEdge = 40.0;
  $nir = 100.0;
  echo rtrim('NDVI=' . _str(ndvi($red, $nir))), PHP_EOL;
  echo rtrim('CCCI=' . _str(ccci($red, $redEdge, $nir))), PHP_EOL;
  echo rtrim('CVI=' . _str(cvi($red, $green, $nir))), PHP_EOL;
}
main();
