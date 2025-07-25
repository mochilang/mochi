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
$__start_mem = memory_get_usage();
$__start = _now();
  function newBitmap($w, $h, $c) {
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
};
  function setPixel(&$b, $x, $y, $c) {
  $rows = $b['pixels'];
  $row = $rows[$y];
  $row[$x] = $c;
  $rows[$y] = $row;
  $b['pixels'] = $rows;
};
  function fillRect(&$b, $x, $y, $w, $h, $c) {
  $yy = $y;
  while ($yy < $y + $h) {
  $xx = $x;
  while ($xx < $x + $w) {
  setPixel($b, $xx, $yy, $c);
  $xx = $xx + 1;
};
  $yy = $yy + 1;
};
};
  function pad($n, $width) {
  $s = _str($n);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
};
  function writePPMP3($b) {
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
  $out = 'P3\n# generated from Bitmap.writeppmp3\n' . _str($b['width']) . ' ' . _str($b['height']) . '\n' . _str($maxv) . '\n';
  $numsize = strlen(_str($maxv));
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
  $out = $out . '\n';
} else {
  $out = $out . '\n';
}
  $y = $y - 1;
};
  return $out;
};
  function main() {
  $black = ['R' => 0, 'G' => 0, 'B' => 0];
  $white = ['R' => 255, 'G' => 255, 'B' => 255];
  $bm = newBitmap(4, 4, $black);
  fillRect($bm, 1, 0, 1, 2, $white);
  setPixel($bm, 3, 3, ['R' => 127, 'G' => 0, 'B' => 63]);
  $ppm = writePPMP3($bm);
  echo rtrim($ppm), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
