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
  function ExampleWriter_WriteBits() {
  $bw = NewWriter('MSB');
  $bw = WriteBits($bw, 15, 4);
  $bw = WriteBits($bw, 0, 1);
  $bw = WriteBits($bw, 19, 5);
  $bw = CloseWriter($bw);
  echo rtrim(bytesToBits($bw['data'])), PHP_EOL;
};
  ExampleWriter_WriteBits();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
