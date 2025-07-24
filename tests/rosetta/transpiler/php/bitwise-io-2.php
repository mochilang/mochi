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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
function pow2($n) {
  global $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $NewReader, $readBitsLSB, $readBitsMSB, $ReadBits, $toBinary, $bytesToBits, $bytesToHex, $mochi_ord, $mochi_chr, $bytesOfStr, $bytesToDec, $Example;
  $v = 1;
  $i = 0;
  while ($i < $n) {
  $v = $v * 2;
  $i = $i + 1;
};
  return $v;
}
function lshift($x, $n) {
  global $pow2, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $NewReader, $readBitsLSB, $readBitsMSB, $ReadBits, $toBinary, $bytesToBits, $bytesToHex, $mochi_ord, $mochi_chr, $bytesOfStr, $bytesToDec, $Example;
  return $x * pow2($n);
}
function rshift($x, $n) {
  global $pow2, $lshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $NewReader, $readBitsLSB, $readBitsMSB, $ReadBits, $toBinary, $bytesToBits, $bytesToHex, $mochi_ord, $mochi_chr, $bytesOfStr, $bytesToDec, $Example;
  return $x / pow2($n);
}
function NewWriter($order) {
  global $pow2, $lshift, $rshift, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $NewReader, $readBitsLSB, $readBitsMSB, $ReadBits, $toBinary, $bytesToBits, $bytesToHex, $mochi_ord, $mochi_chr, $bytesOfStr, $bytesToDec, $Example;
  return ['order' => $order, 'bits' => 0, 'nbits' => 0, 'data' => []];
}
function writeBitsLSB(&$w, $c, $width) {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsMSB, $WriteBits, $CloseWriter, $NewReader, $readBitsLSB, $readBitsMSB, $ReadBits, $toBinary, $bytesToBits, $bytesToHex, $mochi_ord, $mochi_chr, $bytesOfStr, $bytesToDec, $Example;
  $w['bits'] = $w['bits'] + lshift($c, $w['nbits']);
  $w['nbits'] = $w['nbits'] + $width;
  while ($w['nbits'] >= 8) {
  $b = $w['bits'] % 256;
  $w['data'] = array_merge($w['data'], [$b]);
  $w['bits'] = rshift($w['bits'], 8);
  $w['nbits'] = $w['nbits'] - 8;
};
  return $w;
}
function writeBitsMSB(&$w, $c, $width) {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $WriteBits, $CloseWriter, $NewReader, $readBitsLSB, $readBitsMSB, $ReadBits, $toBinary, $bytesToBits, $bytesToHex, $mochi_ord, $mochi_chr, $bytesOfStr, $bytesToDec, $Example;
  $w['bits'] = $w['bits'] + lshift($c, 32 - $width - $w['nbits']);
  $w['nbits'] = $w['nbits'] + $width;
  while ($w['nbits'] >= 8) {
  $b = rshift($w['bits'], 24) % 256;
  $w['data'] = array_merge($w['data'], [$b]);
  $w['bits'] = ($w['bits'] % pow2(24)) * 256;
  $w['nbits'] = $w['nbits'] - 8;
};
  return $w;
}
function WriteBits(&$w, $c, $width) {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $CloseWriter, $NewReader, $readBitsLSB, $readBitsMSB, $ReadBits, $toBinary, $bytesToBits, $bytesToHex, $mochi_ord, $mochi_chr, $bytesOfStr, $bytesToDec, $Example;
  if ($w['order'] == 'LSB') {
  return writeBitsLSB($w, $c, $width);
}
  return writeBitsMSB($w, $c, $width);
}
function CloseWriter(&$w) {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $NewReader, $readBitsLSB, $readBitsMSB, $ReadBits, $toBinary, $bytesToBits, $bytesToHex, $mochi_ord, $mochi_chr, $bytesOfStr, $bytesToDec, $Example;
  if ($w['nbits'] > 0) {
  if ($w['order'] == 'MSB') {
  $w['bits'] = rshift($w['bits'], 24);
};
  $w['data'] = array_merge($w['data'], [$w['bits'] % 256]);
}
  $w['bits'] = 0;
  $w['nbits'] = 0;
  return $w;
}
function NewReader($data, $order) {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $readBitsLSB, $readBitsMSB, $ReadBits, $toBinary, $bytesToBits, $bytesToHex, $mochi_ord, $mochi_chr, $bytesOfStr, $bytesToDec, $Example;
  return ['order' => $order, 'data' => $data, 'idx' => 0, 'bits' => 0, 'nbits' => 0];
}
function readBitsLSB(&$r, $width) {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $NewReader, $readBitsMSB, $ReadBits, $toBinary, $bytesToBits, $bytesToHex, $mochi_ord, $mochi_chr, $bytesOfStr, $bytesToDec, $Example;
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
  $out = $r['bits'] % ($mask + 1);
  $r['bits'] = rshift($r['bits'], $width);
  $r['nbits'] = $r['nbits'] - $width;
  return ['val' => $out, 'eof' => false];
}
function readBitsMSB(&$r, $width) {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $NewReader, $readBitsLSB, $ReadBits, $toBinary, $bytesToBits, $bytesToHex, $mochi_ord, $mochi_chr, $bytesOfStr, $bytesToDec, $Example;
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
  $r['bits'] = ($r['bits'] * pow2($width)) % pow2(32);
  $r['nbits'] = $r['nbits'] - $width;
  return ['val' => $out, 'eof' => false];
}
function ReadBits(&$r, $width) {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $NewReader, $readBitsLSB, $readBitsMSB, $toBinary, $bytesToBits, $bytesToHex, $mochi_ord, $mochi_chr, $bytesOfStr, $bytesToDec, $Example;
  if ($r['order'] == 'LSB') {
  return readBitsLSB($r, $width);
}
  return readBitsMSB($r, $width);
}
function toBinary($n, $bits) {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $NewReader, $readBitsLSB, $readBitsMSB, $ReadBits, $bytesToBits, $bytesToHex, $mochi_ord, $mochi_chr, $bytesOfStr, $bytesToDec, $Example;
  $b = '';
  $val = $n;
  $i = 0;
  while ($i < $bits) {
  $b = _str($val % 2) . $b;
  $val = intdiv($val, 2);
  $i = $i + 1;
};
  return $b;
}
function bytesToBits($bs) {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $NewReader, $readBitsLSB, $readBitsMSB, $ReadBits, $toBinary, $bytesToHex, $mochi_ord, $mochi_chr, $bytesOfStr, $bytesToDec, $Example;
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
}
function bytesToHex($bs) {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $NewReader, $readBitsLSB, $readBitsMSB, $ReadBits, $toBinary, $bytesToBits, $mochi_ord, $mochi_chr, $bytesOfStr, $bytesToDec, $Example;
  $digits = '0123456789ABCDEF';
  $out = '';
  $i = 0;
  while ($i < count($bs)) {
  $b = $bs[$i];
  $hi = intdiv($b, 16);
  $lo = $b % 16;
  $out = $out . substr($digits, $hi, $hi + 1 - $hi) . substr($digits, $lo, $lo + 1 - $lo);
  if ($i + 1 < count($bs)) {
  $out = $out . ' ';
}
  $i = $i + 1;
};
  return $out;
}
function mochi_ord($ch) {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $NewReader, $readBitsLSB, $readBitsMSB, $ReadBits, $toBinary, $bytesToBits, $bytesToHex, $mochi_chr, $bytesOfStr, $bytesToDec, $Example;
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
  return 48 + function($a0) {
  return parseIntStr($ch, $a0);
};
}
  if ($ch == ' ') {
  return 32;
}
  if ($ch == '.') {
  return 46;
}
  return 0;
}
function mochi_chr($n) {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $NewReader, $readBitsLSB, $readBitsMSB, $ReadBits, $toBinary, $bytesToBits, $bytesToHex, $mochi_ord, $bytesOfStr, $bytesToDec, $Example;
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  if ($n >= 65 && $n < 91) {
  return substr($upper, $n - 65, $n - 64 - $n - 65);
}
  if ($n >= 97 && $n < 123) {
  return substr($lower, $n - 97, $n - 96 - $n - 97);
}
  if ($n >= 48 && $n < 58) {
  $digits = '0123456789';
  return substr($digits, $n - 48, $n - 47 - $n - 48);
}
  if ($n == 32) {
  return ' ';
}
  if ($n == 46) {
  return '.';
}
  return '?';
}
function bytesOfStr($s) {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $NewReader, $readBitsLSB, $readBitsMSB, $ReadBits, $toBinary, $bytesToBits, $bytesToHex, $mochi_ord, $mochi_chr, $bytesToDec, $Example;
  $bs = [];
  $i = 0;
  while ($i < strlen($s)) {
  $bs = array_merge($bs, [mochi_ord(substr($s, $i, $i + 1 - $i))]);
  $i = $i + 1;
};
  return $bs;
}
function bytesToDec($bs) {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $NewReader, $readBitsLSB, $readBitsMSB, $ReadBits, $toBinary, $bytesToBits, $bytesToHex, $mochi_ord, $mochi_chr, $bytesOfStr, $Example;
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
}
function Example() {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $NewReader, $readBitsLSB, $readBitsMSB, $ReadBits, $toBinary, $bytesToBits, $bytesToHex, $mochi_ord, $mochi_chr, $bytesOfStr, $bytesToDec;
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
  $v = ord($r['val']);
  if ($v != 0) {
  $result = $result . mochi_chr($v);
}
};
  echo rtrim('Read back as "' . $result . '"'), PHP_EOL;
}
Example();
