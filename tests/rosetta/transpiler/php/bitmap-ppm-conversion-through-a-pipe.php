<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
function pixelFromRgb($c) {
  global $rgbFromPixel, $NewBitmap, $FillRgb, $SetPxRgb, $nextRand, $main;
  $r = (intval((intdiv($c, 65536)))) % 256;
  $g = (intval((intdiv($c, 256)))) % 256;
  $b = $c % 256;
  return ['R' => $r, 'G' => $g, 'B' => $b];
}
function rgbFromPixel($p) {
  global $pixelFromRgb, $NewBitmap, $FillRgb, $SetPxRgb, $nextRand, $main;
  return $p['R'] * 65536 + $p['G'] * 256 + $p['B'];
}
function NewBitmap($x, $y) {
  global $pixelFromRgb, $rgbFromPixel, $FillRgb, $SetPxRgb, $nextRand, $main;
  $data = [];
  $row = 0;
  while ($row < $y) {
  $r = [];
  $col = 0;
  while ($col < $x) {
  $r = array_merge($r, [['R' => 0, 'G' => 0, 'B' => 0]]);
  $col = $col + 1;
};
  $data = array_merge($data, [$r]);
  $row = $row + 1;
};
  return ['cols' => $x, 'rows' => $y, 'px' => $data];
}
function FillRgb(&$b, $c) {
  global $pixelFromRgb, $rgbFromPixel, $NewBitmap, $SetPxRgb, $nextRand, $main;
  $y = 0;
  $p = pixelFromRgb($c);
  while ($y < $b['rows']) {
  $x = 0;
  while ($x < $b['cols']) {
  $px = $b['px'];
  $row = $px[$y];
  $row[$x] = $p;
  $px[$y] = $row;
  $b['px'] = $px;
  $x = $x + 1;
};
  $y = $y + 1;
};
}
function SetPxRgb(&$b, $x, $y, $c) {
  global $pixelFromRgb, $rgbFromPixel, $NewBitmap, $FillRgb, $nextRand, $main;
  if ($x < 0 || $x >= $b['cols'] || $y < 0 || $y >= $b['rows']) {
  return false;
}
  $px = $b['px'];
  $row = $px[$y];
  $row[$x] = pixelFromRgb($c);
  $px[$y] = $row;
  $b['px'] = $px;
  return true;
}
function nextRand($seed) {
  global $pixelFromRgb, $rgbFromPixel, $NewBitmap, $FillRgb, $SetPxRgb, $main;
  return ($seed * 1664525 + 1013904223) % 2147483648;
}
function main() {
  global $pixelFromRgb, $rgbFromPixel, $NewBitmap, $FillRgb, $SetPxRgb, $nextRand;
  $bm = NewBitmap(400, 300);
  FillRgb($bm, 12615744);
  $seed = _now();
  $i = 0;
  while ($i < 2000) {
  $seed = nextRand($seed);
  $x = $seed % 400;
  $seed = nextRand($seed);
  $y = $seed % 300;
  SetPxRgb($bm, $x, $y, 8405024);
  $i = $i + 1;
};
  $x = 0;
  while ($x < 400) {
  $y = 240;
  while ($y < 245) {
  SetPxRgb($bm, $x, $y, 8405024);
  $y = $y + 1;
};
  $y = 260;
  while ($y < 265) {
  SetPxRgb($bm, $x, $y, 8405024);
  $y = $y + 1;
};
  $x = $x + 1;
};
  $y = 0;
  while ($y < 300) {
  $x = 80;
  while ($x < 85) {
  SetPxRgb($bm, $x, $y, 8405024);
  $x = $x + 1;
};
  $x = 95;
  while ($x < 100) {
  SetPxRgb($bm, $x, $y, 8405024);
  $x = $x + 1;
};
  $y = $y + 1;
};
}
main();
