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
  $MOD_ADLER = 65521;
  function mochi_ord($ch) {
  global $MOD_ADLER;
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $digits = '0123456789';
  $i = 0;
  while ($i < strlen($lower)) {
  if (substr($lower, $i, $i + 1 - $i) == $ch) {
  return 97 + $i;
}
  $i = $i + 1;
};
  $i = 0;
  while ($i < strlen($upper)) {
  if (substr($upper, $i, $i + 1 - $i) == $ch) {
  return 65 + $i;
}
  $i = $i + 1;
};
  $i = 0;
  while ($i < strlen($digits)) {
  if (substr($digits, $i, $i + 1 - $i) == $ch) {
  return 48 + $i;
}
  $i = $i + 1;
};
  if ($ch == ' ') {
  return 32;
}
  return 0;
};
  function adler32($plain_text) {
  global $MOD_ADLER;
  $a = 1;
  $b = 0;
  $i = 0;
  while ($i < strlen($plain_text)) {
  $code = mochi_ord(substr($plain_text, $i, $i + 1 - $i));
  $a = ($a + $code) % $MOD_ADLER;
  $b = ($b + $a) % $MOD_ADLER;
  $i = $i + 1;
};
  return $b * 65536 + $a;
};
  function main() {
  global $MOD_ADLER;
  echo rtrim(_str(adler32('Algorithms'))), PHP_EOL;
  echo rtrim(_str(adler32('go adler em all'))), PHP_EOL;
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
