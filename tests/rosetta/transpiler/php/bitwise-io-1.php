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
function pow2($n) {
  global $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $toBinary, $bytesToBits, $ExampleWriter_WriteBits;
  $v = 1;
  $i = 0;
  while ($i < $n) {
  $v = $v * 2;
  $i = $i + 1;
};
  return $v;
}
function lshift($x, $n) {
  global $pow2, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $toBinary, $bytesToBits, $ExampleWriter_WriteBits;
  return $x * pow2($n);
}
function rshift($x, $n) {
  global $pow2, $lshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $toBinary, $bytesToBits, $ExampleWriter_WriteBits;
  return $x / pow2($n);
}
function NewWriter($order) {
  global $pow2, $lshift, $rshift, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $toBinary, $bytesToBits, $ExampleWriter_WriteBits;
  return ['order' => $order, 'bits' => 0, 'nbits' => 0, 'data' => []];
}
function writeBitsLSB(&$w, $c, $width) {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsMSB, $WriteBits, $CloseWriter, $toBinary, $bytesToBits, $ExampleWriter_WriteBits;
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
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $WriteBits, $CloseWriter, $toBinary, $bytesToBits, $ExampleWriter_WriteBits;
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
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $CloseWriter, $toBinary, $bytesToBits, $ExampleWriter_WriteBits;
  if ($w['order'] == 'LSB') {
  return writeBitsLSB($w, $c, $width);
}
  return writeBitsMSB($w, $c, $width);
}
function CloseWriter(&$w) {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $toBinary, $bytesToBits, $ExampleWriter_WriteBits;
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
function toBinary($n, $bits) {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $bytesToBits, $ExampleWriter_WriteBits;
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
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $toBinary, $ExampleWriter_WriteBits;
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
function ExampleWriter_WriteBits() {
  global $pow2, $lshift, $rshift, $NewWriter, $writeBitsLSB, $writeBitsMSB, $WriteBits, $CloseWriter, $toBinary, $bytesToBits;
  $bw = NewWriter('MSB');
  $bw = WriteBits($bw, 15, 4);
  $bw = WriteBits($bw, 0, 1);
  $bw = WriteBits($bw, 19, 5);
  $bw = CloseWriter($bw);
  echo rtrim(bytesToBits($bw['data'])), PHP_EOL;
}
ExampleWriter_WriteBits();
