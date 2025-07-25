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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function pow2($n) {
  $v = 1;
  $i = 0;
  while ($i < $n) {
  $v = $v * 2;
  $i = $i + 1;
};
  return $v;
};
  function lshift($x, $n) {
  return $x * pow2($n);
};
  function rshift($x, $n) {
  return $x / pow2($n);
};
  function NewWriter($order) {
  return ['order' => $order, 'bits' => 0, 'nbits' => 0, 'data' => []];
};
  function writeBitsLSB(&$w, $c, $width) {
  $w['bits'] = $w['bits'] + lshift($c, $w['nbits']);
  $w['nbits'] = $w['nbits'] + $width;
  while ($w['nbits'] >= 8) {
  $b = fmod($w['bits'], 256);
  $w['data'] = array_merge($w['data'], [$b]);
  $w['bits'] = rshift($w['bits'], 8);
  $w['nbits'] = $w['nbits'] - 8;
};
  return $w;
};
  function writeBitsMSB(&$w, $c, $width) {
  $w['bits'] = $w['bits'] + lshift($c, 32 - $width - $w['nbits']);
  $w['nbits'] = $w['nbits'] + $width;
  while ($w['nbits'] >= 8) {
  $b = fmod(rshift($w['bits'], 24), 256);
  $w['data'] = array_merge($w['data'], [$b]);
  $w['bits'] = (fmod($w['bits'], pow2(24))) * 256;
  $w['nbits'] = $w['nbits'] - 8;
};
  return $w;
};
  function WriteBits(&$w, $c, $width) {
  if ($w['order'] == 'LSB') {
  return writeBitsLSB($w, $c, $width);
}
  return writeBitsMSB($w, $c, $width);
};
  function CloseWriter(&$w) {
  if ($w['nbits'] > 0) {
  if ($w['order'] == 'MSB') {
  $w['bits'] = rshift($w['bits'], 24);
};
  $w['data'] = array_merge($w['data'], [fmod($w['bits'], 256)]);
}
  $w['bits'] = 0;
  $w['nbits'] = 0;
  return $w;
};
  function NewReader($data, $order) {
  return ['order' => $order, 'data' => $data, 'idx' => 0, 'bits' => 0, 'nbits' => 0];
};
  function readBitsLSB(&$r, $width) {
  while ($r['nbits'] < $width) {
  if ($r['idx'] >= _len($r['data'])) {
  return ['val' => 0, 'eof' => true];
}
  $b = $r['data'][$r['idx']];
  $r['idx'] = $r['idx'] + 1;
  $r['bits'] = $r['bits'] + lshift($b, $r['nbits']);
  $r['nbits'] = $r['nbits'] + 8;
};
  $mask = pow2($width) - 1;
  $out = fmod($r['bits'], ($mask + 1));
  $r['bits'] = rshift($r['bits'], $width);
  $r['nbits'] = $r['nbits'] - $width;
  return ['val' => $out, 'eof' => false];
};
  function readBitsMSB(&$r, $width) {
  while ($r['nbits'] < $width) {
  if ($r['idx'] >= _len($r['data'])) {
  return ['val' => 0, 'eof' => true];
}
  $b = $r['data'][$r['idx']];
  $r['idx'] = $r['idx'] + 1;
  $r['bits'] = $r['bits'] + lshift($b, 24 - $r['nbits']);
  $r['nbits'] = $r['nbits'] + 8;
};
  $out = rshift($r['bits'], 32 - $width);
  $r['bits'] = fmod(($r['bits'] * pow2($width)), pow2(32));
  $r['nbits'] = $r['nbits'] - $width;
  return ['val' => $out, 'eof' => false];
};
  function ReadBits(&$r, $width) {
  if ($r['order'] == 'LSB') {
  return readBitsLSB($r, $width);
}
  return readBitsMSB($r, $width);
};
  function toBinary($n, $bits) {
  $b = '';
  $val = $n;
  $i = 0;
  while ($i < $bits) {
  $b = _str($val % 2) . $b;
  $val = _intdiv($val, 2);
  $i = $i + 1;
};
  return $b;
};
  function bytesToBits($bs) {
  $out = '[';
  $i = 0;
  while ($i < count($bs)) {
  $out = $out . toBinary($bs[$i], 8);
  if ($i + 1 < count($bs)) {
  $out = $out . ' ';
}
  $i = $i + 1;
};
  $out = $out . ']';
  return $out;
};
  function bytesToHex($bs) {
  $digits = '0123456789ABCDEF';
  $out = '';
  $i = 0;
  while ($i < count($bs)) {
  $b = $bs[$i];
  $hi = _intdiv($b, 16);
  $lo = $b % 16;
  $out = $out . substr($digits, $hi, $hi + 1 - $hi) . substr($digits, $lo, $lo + 1 - $lo);
  if ($i + 1 < count($bs)) {
  $out = $out . ' ';
}
  $i = $i + 1;
};
  return $out;
};
  function mochi_ord($ch) {
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $idx = _indexof($upper, $ch);
  if ($idx >= 0) {
  return 65 + $idx;
}
  $idx = _indexof($lower, $ch);
  if ($idx >= 0) {
  return 97 + $idx;
}
  if ($ch >= '0' && $ch <= '9') {
  return 48 + parseIntStr($ch, 10);
}
  if ($ch == ' ') {
  return 32;
}
  if ($ch == '.') {
  return 46;
}
  return 0;
};
  function mochi_chr($n) {
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  if ($n >= 65 && $n < 91) {
  return substr($upper, $n - 65, $n - 64 - ($n - 65));
}
  if ($n >= 97 && $n < 123) {
  return substr($lower, $n - 97, $n - 96 - ($n - 97));
}
  if ($n >= 48 && $n < 58) {
  $digits = '0123456789';
  return substr($digits, $n - 48, $n - 47 - ($n - 48));
}
  if ($n == 32) {
  return ' ';
}
  if ($n == 46) {
  return '.';
}
  return '?';
};
  function bytesOfStr($s) {
  $bs = [];
  $i = 0;
  while ($i < strlen($s)) {
  $bs = array_merge($bs, [mochi_ord(substr($s, $i, $i + 1 - $i))]);
  $i = $i + 1;
};
  return $bs;
};
  function bytesToDec($bs) {
  $out = '';
  $i = 0;
  while ($i < count($bs)) {
  $out = $out . _str($bs[$i]);
  if ($i + 1 < count($bs)) {
  $out = $out . ' ';
}
  $i = $i + 1;
};
  return $out;
};
  function Example() {
  $message = 'This is a test.';
  $msgBytes = bytesOfStr($message);
  echo rtrim('"' . $message . '" as bytes: ' . bytesToDec($msgBytes)), PHP_EOL;
  echo rtrim('    original bits: ' . bytesToBits($msgBytes)), PHP_EOL;
  $bw = NewWriter('MSB');
  $i = 0;
  while ($i < count($msgBytes)) {
  $bw = WriteBits($bw, $msgBytes[$i], 7);
  $i = $i + 1;
};
  $bw = CloseWriter($bw);
  echo rtrim('Written bitstream: ' . bytesToBits($bw['data'])), PHP_EOL;
  echo rtrim('Written bytes: ' . bytesToHex($bw['data'])), PHP_EOL;
  $br = NewReader($bw['data'], 'MSB');
  $result = '';
  while (true) {
  $r = ReadBits($br, 7);
  if ($r['eof']) {
  break;
}
  $v = intval($r['val']);
  if ($v != 0) {
  $result = $result . mochi_chr($v);
}
};
  echo rtrim('Read back as "' . $result . '"'), PHP_EOL;
};
  Example();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
