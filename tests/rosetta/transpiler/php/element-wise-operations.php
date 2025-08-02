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
  function pow10($n) {
  $r = 1.0;
  $i = 0;
  while ($i < $n) {
  $r = $r * 10.0;
  $i = $i + 1;
};
  return $r;
};
  function powf($base, $exp) {
  if ('mochi_exp' == 0.5) {
  $guess = $base;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $base / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
  $result = 1.0;
  $n = intval('mochi_exp');
  $i = 0;
  while ($i < $n) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function formatFloat($f, $prec) {
  $scale = pow10($prec);
  $scaled = ($f * $scale) + 0.5;
  $n = (intval($scaled));
  $digits = _str($n);
  while (strlen($digits) <= $prec) {
  $digits = '0' . $digits;
};
  $intPart = substr($digits, 0, strlen($digits) - $prec - 0);
  $fracPart = substr($digits, strlen($digits) - $prec, strlen($digits) - (strlen($digits) - $prec));
  return $intPart . '.' . $fracPart;
};
  function padLeft($s, $w) {
  $res = '';
  $n = $w - strlen($s);
  while ($n > 0) {
  $res = $res . ' ';
  $n = $n - 1;
};
  return $res . $s;
};
  function rowString($row) {
  $s = '[';
  $i = 0;
  while ($i < count($row)) {
  $s = $s . padLeft(formatFloat($row[$i], 3), 6);
  if ($i < count($row) - 1) {
  $s = $s . ' ';
}
  $i = $i + 1;
};
  return $s . '] ';
};
  function printMatrix($heading, $m) {
  echo rtrim($heading), PHP_EOL;
  $i = 0;
  while ($i < count($m)) {
  echo rtrim(rowString($m[$i])), PHP_EOL;
  $i = $i + 1;
};
};
  function elementWiseMM($m1, $m2, $f) {
  $z = [];
  $r = 0;
  while ($r < count($m1)) {
  $row = [];
  $c = 0;
  while ($c < count($m1[$r])) {
  $row = array_merge($row, [$f($m1[$r][$c], $m2[$r][$c])]);
  $c = $c + 1;
};
  $z = array_merge($z, [$row]);
  $r = $r + 1;
};
  return $z;
};
  function elementWiseMS($m, $s, $f) {
  $z = [];
  $r = 0;
  while ($r < count($m)) {
  $row = [];
  $c = 0;
  while ($c < count($m[$r])) {
  $row = array_merge($row, [$f($m[$r][$c], $s)]);
  $c = $c + 1;
};
  $z = array_merge($z, [$row]);
  $r = $r + 1;
};
  return $z;
};
  function add($a, $b) {
  return $a + $b;
};
  function sub($a, $b) {
  return $a - $b;
};
  function mul($a, $b) {
  return $a * $b;
};
  function div($a, $b) {
  return $a / $b;
};
  function mochi_exp($a, $b) {
  return powf($a, $b);
};
  function main() {
  $m1 = [[3.0, 1.0, 4.0], [1.0, 5.0, 9.0]];
  $m2 = [[2.0, 7.0, 1.0], [8.0, 2.0, 8.0]];
  printMatrix('m1:', $m1);
  printMatrix('m2:', $m2);
  echo rtrim(''), PHP_EOL;
  printMatrix('m1 + m2:', elementWiseMM($m1, $m2, 'add'));
  printMatrix('m1 - m2:', elementWiseMM($m1, $m2, 'sub'));
  printMatrix('m1 * m2:', elementWiseMM($m1, $m2, 'mul'));
  printMatrix('m1 / m2:', elementWiseMM($m1, $m2, 'div'));
  printMatrix('m1 ^ m2:', elementWiseMM($m1, $m2, 'mochi_exp'));
  echo rtrim(''), PHP_EOL;
  $s = 0.5;
  echo rtrim('s: ' . _str($s)), PHP_EOL;
  printMatrix('m1 + s:', elementWiseMS($m1, $s, 'add'));
  printMatrix('m1 - s:', elementWiseMS($m1, $s, 'sub'));
  printMatrix('m1 * s:', elementWiseMS($m1, $s, 'mul'));
  printMatrix('m1 / s:', elementWiseMS($m1, $s, 'div'));
  printMatrix('m1 ^ s:', elementWiseMS($m1, $s, 'mochi_exp'));
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
