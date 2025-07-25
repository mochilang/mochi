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
  function mochi_parseIntStr($str) {
  global $ppmData, $img;
  $i = 0;
  $neg = false;
  if (strlen($str) > 0 && substr($str, 0, 1 - 0) == '-') {
  $neg = true;
  $i = 1;
}
  $n = 0;
  $digits = ['0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9];
  while ($i < strlen($str)) {
  $n = $n * 10 + $digits[substr($str, $i, $i + 1 - $i)];
  $i = $i + 1;
};
  if ($neg) {
  $n = -$n;
}
  return $n;
};
  function splitWs($s) {
  global $ppmData, $img;
  $parts = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == ' ' || $ch == '\n' || $ch == '\t' || $ch == '') {
};
  function parsePpm($data) {
  global $ppmData, $img;
  $w = parseIntStr($toks[1], 10);
  $h = parseIntStr($toks[2], 10);
  $maxv = parseIntStr($toks[3], 10);
  $px = array_merge($px, [parseIntStr($toks[$i], 10)]);
};
  $ppmData = 'P3\n2 2\n1\n0 1 1 0 1 0 0 1 1 1 0 0\n';
  $img = parsePpm($ppmData);
  echo rtrim('width=' . _str($img['w']) . ' height=' . _str($img['h'])), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
function parsePpm($data) {
  global $parseIntStr, $splitWs, $ppmData, $img;
  $toks = splitWs($data);
  if (count($toks) < 4) {
  return ['err' => true];
}
  $magic = $toks[0];
  $w = parseIntStr($toks[1]);
  $h = parseIntStr($toks[2]);
  $maxv = parseIntStr($toks[3]);
  $px = [];
  $i = 4;
  while ($i < count($toks)) {
  $px = array_merge($px, [parseIntStr($toks[$i])]);
  $i = $i + 1;
};
  return ['magic' => $magic, 'w' => $w, 'h' => $h, 'max' => $maxv, 'px' => $px];
}
$ppmData = 'P3
2 2
1
0 1 1 0 1 0 0 1 1 1 0 0
';
$img = parsePpm($ppmData);
echo rtrim('width=' . _str($img['w']) . ' height=' . _str($img['h'])), PHP_EOL;
