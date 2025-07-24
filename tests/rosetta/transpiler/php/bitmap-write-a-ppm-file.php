<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
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
function newBitmap($w, $h, $c) {
  global $setPixel, $fillRect, $pad, $writePPMP3, $main;
  $rows = [];
  $y = 0;
  while ($y < $h) {
  $row = [];
  $x = 0;
  while ($x < $w) {
  $row = array_merge($row, [$c]);
  $x = $x + 1;
};
  $rows = array_merge($rows, [$row]);
  $y = $y + 1;
};
  return ['width' => $w, 'height' => $h, 'pixels' => $rows];
}
function setPixel(&$b, $x, $y, $c) {
  global $newBitmap, $fillRect, $pad, $writePPMP3, $main;
  $rows = $b['pixels'];
  $row = $rows[$y];
  $row[$x] = $c;
  $rows[$y] = $row;
  $b['pixels'] = $rows;
}
function fillRect(&$b, $x, $y, $w, $h, $c) {
  global $newBitmap, $setPixel, $pad, $writePPMP3, $main;
  $yy = $y;
  while ($yy < $y + $h) {
  $xx = $x;
  while ($xx < $x + $w) {
  setPixel($b, $xx, $yy, $c);
  $xx = $xx + 1;
};
  $yy = $yy + 1;
};
}
function pad($n, $width) {
  global $newBitmap, $setPixel, $fillRect, $writePPMP3, $main;
  $s = _str($n);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
}
function writePPMP3($b) {
  global $newBitmap, $setPixel, $fillRect, $pad, $main;
  $maxv = 0;
  $y = 0;
  while ($y < $b['height']) {
  $x = 0;
  while ($x < $b['width']) {
  $p = $b['pixels'][$y][$x];
  if ($p['R'] > $maxv) {
  $maxv = $p['R'];
}
  if ($p['G'] > $maxv) {
  $maxv = $p['G'];
}
  if ($p['B'] > $maxv) {
  $maxv = $p['B'];
}
  $x = $x + 1;
};
  $y = $y + 1;
};
  $out = 'P3
# generated from Bitmap.writeppmp3
' . _str($b['width']) . ' ' . _str($b['height']) . '
' . _str($maxv) . '
';
  $numsize = _len(_str($maxv));
  $y = $b['height'] - 1;
  while ($y >= 0) {
  $line = '';
  $x = 0;
  while ($x < $b['width']) {
  $p = $b['pixels'][$y][$x];
  $line = $line . '   ' . pad($p['R'], $numsize) . ' ' . pad($p['G'], $numsize) . ' ' . pad($p['B'], $numsize);
  $x = $x + 1;
};
  $out = $out . $line;
  if ($y > 0) {
  $out = $out . '
';
} else {
  $out = $out . '
';
}
  $y = $y - 1;
};
  return $out;
}
function main() {
  global $newBitmap, $setPixel, $fillRect, $pad, $writePPMP3;
  $black = ['R' => 0, 'G' => 0, 'B' => 0];
  $white = ['R' => 255, 'G' => 255, 'B' => 255];
  $bm = newBitmap(4, 4, $black);
  fillRect($bm, 1, 0, 1, 2, $white);
  setPixel($bm, 3, 3, ['R' => 127, 'G' => 0, 'B' => 63]);
  $ppm = writePPMP3($bm);
  echo rtrim($ppm), PHP_EOL;
}
main();
