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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function dbRec($k, $n, $t, $p, &$a, $seq) {
  if ($t > $n) {
  if ($n % $p == 0) {
  $j = 1;
  while ($j <= $p) {
  $seq = _append($seq, $a[$j]);
  $j = $j + 1;
};
};
} else {
  $a[$t] = $a[$t - $p];
  $seq = dbRec($k, $n, $t + 1, $p, $a, $seq);
  $j = $a[$t - $p] + 1;
  while ($j < $k) {
  $a[$t] = $j;
  $seq = dbRec($k, $n, $t + 1, $t, $a, $seq);
  $j = $j + 1;
};
}
  return $seq;
};
  function deBruijn($k, $n) {
  $digits = '0123456789';
  $alphabet = $digits;
  if ($k < 10) {
  $alphabet = substr($digits, 0, $k - 0);
}
  $a = [];
  $i = 0;
  while ($i < $k * $n) {
  $a = _append($a, 0);
  $i = $i + 1;
};
  $seq = [];
  $seq = dbRec($k, $n, 1, 1, $a, $seq);
  $b = '';
  $idx = 0;
  while ($idx < count($seq)) {
  $b = $b . substr($alphabet, $seq[$idx], $seq[$idx] + 1 - $seq[$idx]);
  $idx = $idx + 1;
};
  $b = $b . substr($b, 0, $n - 1 - 0);
  return $b;
};
  function allDigits($s) {
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch < '0' || $ch > '9') {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function mochi_parseIntStr($str) {
  $n = 0;
  $i = 0;
  while ($i < strlen($str)) {
  $n = $n * 10 + (ord(substr($str, $i, $i + 1 - $i)));
  $i = $i + 1;
};
  return $n;
};
  function validate($db) {
  $le = strlen($db);
  $found = [];
  $i = 0;
  while ($i < 10000) {
  $found = _append($found, 0);
  $i = $i + 1;
};
  $j = 0;
  while ($j < $le - 3) {
  $s = substr($db, $j, $j + 4 - $j);
  if (allDigits($s)) {
  $n = parseIntStr($s, 10);
  $found[$n] = $found[$n] + 1;
}
  $j = $j + 1;
};
  $errs = [];
  $k = 0;
  while ($k < 10000) {
  if ($found[$k] == 0) {
  $errs = _append($errs, '    PIN number ' . padLeft($k, 4) . ' missing');
} else {
  if ($found[$k] > 1) {
  $errs = _append($errs, '    PIN number ' . padLeft($k, 4) . ' occurs ' . _str($found[$k]) . ' times');
};
}
  $k = $k + 1;
};
  $lerr = count($errs);
  if ($lerr == 0) {
  echo rtrim('  No errors found'), PHP_EOL;
} else {
  $pl = 's';
  if ($lerr == 1) {
  $pl = '';
};
  echo rtrim('  ' . _str($lerr) . ' error' . $pl . ' found:'), PHP_EOL;
  $msg = joinStr($errs, '
');
  echo rtrim($msg), PHP_EOL;
}
};
  function padLeft($n, $width) {
  $s = _str($n);
  while (strlen($s) < $width) {
  $s = '0' . $s;
};
  return $s;
};
  function joinStr($xs, $sep) {
  $res = '';
  $i = 0;
  while ($i < count($xs)) {
  if ($i > 0) {
  $res = $res . $sep;
}
  $res = $res . $xs[$i];
  $i = $i + 1;
};
  return $res;
};
  function reverse($s) {
  $out = '';
  $i = strlen($s) - 1;
  while ($i >= 0) {
  $out = $out . substr($s, $i, $i + 1 - $i);
  $i = $i - 1;
};
  return $out;
};
  function main() {
  $db = deBruijn(10, 4);
  $le = strlen($db);
  echo rtrim('The length of the de Bruijn sequence is ' . _str($le)), PHP_EOL;
  echo rtrim('
The first 130 digits of the de Bruijn sequence are:'), PHP_EOL;
  echo rtrim(substr($db, 0, 130 - 0)), PHP_EOL;
  echo rtrim('
The last 130 digits of the de Bruijn sequence are:'), PHP_EOL;
  echo rtrim(substr($db, $le - 130)), PHP_EOL;
  echo rtrim('
Validating the de Bruijn sequence:'), PHP_EOL;
  validate($db);
  echo rtrim('
Validating the reversed de Bruijn sequence:'), PHP_EOL;
  $dbr = reverse($db);
  validate($dbr);
  $db = substr($db, 0, 4443 - 0) . '.' . substr($db, 4444, strlen($db) - 4444);
  echo rtrim('
Validating the overlaid de Bruijn sequence:'), PHP_EOL;
  validate($db);
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
