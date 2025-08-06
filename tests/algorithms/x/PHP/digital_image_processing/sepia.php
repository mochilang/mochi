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
function normalize($value) {
  global $image, $sepia;
  if ($value > 255) {
  return 255;
}
  return $value;
}
function to_grayscale($blue, $green, $red) {
  global $image, $sepia;
  $gs = 0.2126 * (floatval($red)) + 0.587 * (floatval($green)) + 0.114 * (floatval($blue));
  return intval($gs);
}
function make_sepia(&$img, $factor) {
  global $image, $sepia;
  $pixel_h = count($img);
  $pixel_v = count($img[0]);
  $i = 0;
  while ($i < $pixel_h) {
  $j = 0;
  while ($j < $pixel_v) {
  $pixel = $img[$i][$j];
  $grey = to_grayscale($pixel[0], $pixel[1], $pixel[2]);
  $img[$i][$j] = [normalize($grey), normalize($grey + $factor), normalize($grey + 2 * $factor)];
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $img;
}
$image = [[[10, 20, 30], [40, 50, 60]], [[70, 80, 90], [200, 150, 100]]];
$sepia = make_sepia($image, 20);
echo rtrim(_str($sepia)), PHP_EOL;
