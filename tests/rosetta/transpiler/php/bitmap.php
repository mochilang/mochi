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
function pixelFromRgb($c) {
  global $rgbFromPixel, $NewBitmap, $Extent, $Fill, $FillRgb, $SetPx, $SetPxRgb, $GetPx, $GetPxRgb, $ppmSize, $pixelStr, $main;
  $r = (intval((intdiv($c, 65536)))) % 256;
  $g = (intval((intdiv($c, 256)))) % 256;
  $b = $c % 256;
  return ['R' => $r, 'G' => $g, 'B' => $b];
}
function rgbFromPixel($p) {
  global $pixelFromRgb, $NewBitmap, $Extent, $Fill, $FillRgb, $SetPx, $SetPxRgb, $GetPx, $GetPxRgb, $ppmSize, $pixelStr, $main;
  return $p['R'] * 65536 + $p['G'] * 256 + $p['B'];
}
function NewBitmap($x, $y) {
  global $pixelFromRgb, $rgbFromPixel, $Extent, $Fill, $FillRgb, $SetPx, $SetPxRgb, $GetPx, $GetPxRgb, $ppmSize, $pixelStr, $main;
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
function Extent($b) {
  global $pixelFromRgb, $rgbFromPixel, $NewBitmap, $Fill, $FillRgb, $SetPx, $SetPxRgb, $GetPx, $GetPxRgb, $ppmSize, $pixelStr, $main;
  return ['cols' => $b['cols'], 'rows' => $b['rows']];
}
function Fill(&$b, $p) {
  global $pixelFromRgb, $rgbFromPixel, $NewBitmap, $Extent, $FillRgb, $SetPx, $SetPxRgb, $GetPx, $GetPxRgb, $ppmSize, $pixelStr, $main;
  $y = 0;
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
function FillRgb(&$b, $c) {
  global $pixelFromRgb, $rgbFromPixel, $NewBitmap, $Extent, $Fill, $SetPx, $SetPxRgb, $GetPx, $GetPxRgb, $ppmSize, $pixelStr, $main;
  Fill($b, pixelFromRgb($c));
}
function SetPx(&$b, $x, $y, $p) {
  global $pixelFromRgb, $rgbFromPixel, $NewBitmap, $Extent, $Fill, $FillRgb, $SetPxRgb, $GetPx, $GetPxRgb, $ppmSize, $pixelStr, $main;
  if ($x < 0 || $x >= $b['cols'] || $y < 0 || $y >= $b['rows']) {
  return false;
}
  $px = $b['px'];
  $row = $px[$y];
  $row[$x] = $p;
  $px[$y] = $row;
  $b['px'] = $px;
  return true;
}
function SetPxRgb(&$b, $x, $y, $c) {
  global $pixelFromRgb, $rgbFromPixel, $NewBitmap, $Extent, $Fill, $FillRgb, $SetPx, $GetPx, $GetPxRgb, $ppmSize, $pixelStr, $main;
  return SetPx($b, $x, $y, pixelFromRgb($c));
}
function GetPx($b, $x, $y) {
  global $pixelFromRgb, $rgbFromPixel, $NewBitmap, $Extent, $Fill, $FillRgb, $SetPx, $SetPxRgb, $GetPxRgb, $ppmSize, $pixelStr, $main;
  if ($x < 0 || $x >= $b['cols'] || $y < 0 || $y >= $b['rows']) {
  return ['ok' => false];
}
  $row = $b['px'][$y];
  return ['ok' => true, 'pixel' => $row[$x]];
}
function GetPxRgb($b, $x, $y) {
  global $pixelFromRgb, $rgbFromPixel, $NewBitmap, $Extent, $Fill, $FillRgb, $SetPx, $SetPxRgb, $GetPx, $ppmSize, $pixelStr, $main;
  $r = GetPx($b, $x, $y);
  if (!$r['ok']) {
  return ['ok' => false];
}
  return ['ok' => true, 'rgb' => rgbFromPixel($r['pixel'])];
}
function ppmSize($b) {
  global $pixelFromRgb, $rgbFromPixel, $NewBitmap, $Extent, $Fill, $FillRgb, $SetPx, $SetPxRgb, $GetPx, $GetPxRgb, $pixelStr, $main;
  $header = 'P6
# Creator: Rosetta Code http://rosettacode.org/
' . _str($b['cols']) . ' ' . _str($b['rows']) . '
255
';
  return strlen($header) + 3 * $b['cols'] * $b['rows'];
}
function pixelStr($p) {
  global $pixelFromRgb, $rgbFromPixel, $NewBitmap, $Extent, $Fill, $FillRgb, $SetPx, $SetPxRgb, $GetPx, $GetPxRgb, $ppmSize, $main;
  return '{' . _str($p['R']) . ' ' . _str($p['G']) . ' ' . _str($p['B']) . '}';
}
function main() {
  global $pixelFromRgb, $rgbFromPixel, $NewBitmap, $Extent, $Fill, $FillRgb, $SetPx, $SetPxRgb, $GetPx, $GetPxRgb, $ppmSize, $pixelStr;
  $bm = NewBitmap(300, 240);
  FillRgb($bm, 16711680);
  SetPxRgb($bm, 10, 20, 255);
  SetPxRgb($bm, 20, 30, 0);
  SetPxRgb($bm, 30, 40, 1056816);
  $c1 = GetPx($bm, 0, 0);
  $c2 = GetPx($bm, 10, 20);
  $c3 = GetPx($bm, 30, 40);
  echo rtrim('Image size: ' . _str($bm['cols']) . ' × ' . _str($bm['rows'])), PHP_EOL;
  echo rtrim(_str(ppmSize($bm)) . ' bytes when encoded as PPM.'), PHP_EOL;
  if ($c1['ok']) {
  echo rtrim('Pixel at (0,0) is ' . pixelStr($c1['pixel'])), PHP_EOL;
}
  if ($c2['ok']) {
  echo rtrim('Pixel at (10,20) is ' . pixelStr($c2['pixel'])), PHP_EOL;
}
  if ($c3['ok']) {
  $p = $c3['pixel'];
  $r16 = $p['R'] * 257;
  $g16 = $p['G'] * 257;
  $b16 = $p['B'] * 257;
  echo rtrim('Pixel at (30,40) has R=' . _str($r16) . ', G=' . _str($g16) . ', B=' . _str($b16)), PHP_EOL;
}
}
main();
