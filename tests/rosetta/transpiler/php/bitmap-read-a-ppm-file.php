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
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function newBitmap($w, $h, $max) {
  global $ppmtxt, $bm, $out;
  $rows = [];
  $y = 0;
  while ($y < $h) {
  $row = [];
  $x = 0;
  while ($x < $w) {
  $row = array_merge($row, [['R' => 0, 'G' => 0, 'B' => 0]]);
  $x = $x + 1;
};
  $rows = array_merge($rows, [$row]);
  $y = $y + 1;
};
  return ['w' => $w, 'h' => $h, 'max' => $max, 'data' => $rows];
};
  function setPx(&$b, $x, $y, $p) {
  global $ppmtxt, $bm, $out;
  $rows = $b['data'];
  $row = $rows[$y];
  $row[$x] = $p;
  $rows[$y] = $row;
  $b['data'] = $rows;
};
  function getPx($b, $x, $y) {
  global $ppmtxt, $bm, $out;
  return $b['data'][$y][$x];
};
  function splitLines($s) {
  global $ppmtxt, $bm;
  $out = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == '\n') {
  $out = array_merge($out, [$cur]);
  $cur = '';
} else {
  $cur = $cur . $ch;
}
  $i = $i + 1;
};
  $out = array_merge($out, [$cur]);
  return $out;
};
  function splitWS($s) {
  global $ppmtxt, $bm;
  $out = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == ' ' || $ch == '\t' || $ch == '' || $ch == '\n') {
};
  function mochi_parseIntStr($str) {
  global $ppmtxt, $bm, $out;
};
  function tokenize($s) {
  global $ppmtxt, $bm, $out;
  if (strlen($line) > 0 && substr($line, 0, 1 - 0) == '#') {
}
  $i = $i + 1;
};
};
  function readP3($text) {
  global $ppmtxt, $out;
  $w = parseIntStr($toks[1], 10);
  $h = parseIntStr($toks[2], 10);
  $maxv = parseIntStr($toks[3], 10);
  $r = parseIntStr($toks[$idx], 10);
  $g = parseIntStr($toks[$idx + 1], 10);
  $b = parseIntStr($toks[$idx + 2], 10);
};
  function toGrey(&$b) {
  global $ppmtxt, $bm, $out;
}
};
  function pad($n, $w) {
  global $ppmtxt, $bm, $out;
};
  function writeP3($b) {
  global $ppmtxt, $bm;
  $digits = strlen(_str($max));
  $out = 'P3\n# generated from Bitmap.writeppmp3\n' . _str($w) . ' ' . _str($h) . '\n' . _str($max) . '\n';
  $out = $out . $line . '\n';
};
  $ppmtxt = 'P3\n' . '# feep.ppm\n' . '4 4\n' . '15\n' . ' 0  0  0    0  0  0    0  0  0   15  0 15\n' . ' 0  0  0    0 15  7    0  0  0    0  0  0\n' . ' 0  0  0    0  0  0    0 15  7    0  0  0\n' . '15  0 15    0  0  0    0  0  0    0  0  0\n';
  echo rtrim('Original Colour PPM file'), PHP_EOL;
  echo rtrim($ppmtxt), PHP_EOL;
  $bm = readP3($ppmtxt);
  echo rtrim('Grey PPM:'), PHP_EOL;
  toGrey($bm);
  $out = writeP3($bm);
  echo rtrim($out), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
}
function readP3($text) {
  global $newBitmap, $setPx, $getPx, $splitLines, $splitWS, $parseIntStr, $tokenize, $toGrey, $pad, $writeP3, $ppmtxt, $out;
  $toks = tokenize($text);
  if (count($toks) < 4) {
  return newBitmap(0, 0, 0);
}
  if ($toks[0] != 'P3') {
  return newBitmap(0, 0, 0);
}
  $w = parseIntStr($toks[1]);
  $h = parseIntStr($toks[2]);
  $maxv = parseIntStr($toks[3]);
  $idx = 4;
  $bm = newBitmap($w, $h, $maxv);
  $y = $h - 1;
  while ($y >= 0) {
  $x = 0;
  while ($x < $w) {
  $r = parseIntStr($toks[$idx]);
  $g = parseIntStr($toks[$idx + 1]);
  $b = parseIntStr($toks[$idx + 2]);
  setPx($bm, $x, $y, ['R' => $r, 'G' => $g, 'B' => $b]);
  $idx = $idx + 3;
  $x = $x + 1;
};
  $y = $y - 1;
};
  return $bm;
}
function toGrey(&$b) {
  global $newBitmap, $setPx, $getPx, $splitLines, $splitWS, $parseIntStr, $tokenize, $readP3, $pad, $writeP3, $ppmtxt, $bm, $out;
  $h = $b['h'];
  $w = $b['w'];
  $m = 0;
  $y = 0;
  while ($y < $h) {
  $x = 0;
  while ($x < $w) {
  $p = getPx($b, $x, $y);
  $l = ($p['R'] * 2126 + $p['G'] * 7152 + $p['B'] * 722) / 10000;
  if ($l > $b['max']) {
  $l = $b['max'];
}
  setPx($b, $x, $y, ['R' => $l, 'G' => $l, 'B' => $l]);
  if ($l > $m) {
  $m = $l;
}
  $x = $x + 1;
};
  $y = $y + 1;
};
  $b['max'] = $m;
}
function pad($n, $w) {
  global $newBitmap, $setPx, $getPx, $splitLines, $splitWS, $parseIntStr, $tokenize, $readP3, $toGrey, $writeP3, $ppmtxt, $bm, $out;
  $s = _str($n);
  while (strlen($s) < $w) {
  $s = ' ' . $s;
};
  return $s;
}
function writeP3($b) {
  global $newBitmap, $setPx, $getPx, $splitLines, $splitWS, $parseIntStr, $tokenize, $readP3, $toGrey, $pad, $ppmtxt, $bm;
  $h = $b['h'];
  $w = $b['w'];
  $max = $b['max'];
  $digits = _len(_str($max));
  $out = 'P3
# generated from Bitmap.writeppmp3
' . _str($w) . ' ' . _str($h) . '
' . _str($max) . '
';
  $y = $h - 1;
  while ($y >= 0) {
  $line = '';
  $x = 0;
  while ($x < $w) {
  $p = getPx($b, $x, $y);
  $line = $line . '   ' . pad($p['R'], $digits) . ' ' . pad($p['G'], $digits) . ' ' . pad($p['B'], $digits);
  $x = $x + 1;
};
  $out = $out . $line . '
';
  $y = $y - 1;
};
  return $out;
}
$ppmtxt = 'P3
' . '# feep.ppm
' . '4 4
' . '15
' . ' 0  0  0    0  0  0    0  0  0   15  0 15
' . ' 0  0  0    0 15  7    0  0  0    0  0  0
' . ' 0  0  0    0  0  0    0 15  7    0  0  0
' . '15  0 15    0  0  0    0  0  0    0  0  0
';
echo rtrim('Original Colour PPM file'), PHP_EOL;
echo rtrim($ppmtxt), PHP_EOL;
$bm = readP3($ppmtxt);
echo rtrim('Grey PPM:'), PHP_EOL;
toGrey($bm);
$out = writeP3($bm);
echo rtrim($out), PHP_EOL;
