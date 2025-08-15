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
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function _sha256($bs) {
    $bin = '';
    foreach ($bs as $b) { $bin .= chr($b); }
    $hash = hash('sha256', $bin, true);
    return array_values(unpack('C*', $hash));
}
$__start_mem = memory_get_usage();
$__start = _now();
  $HEX = '0123456789abcdef';
  function byte_to_hex($b) {
  global $HEX;
  $hi = _intdiv($b, 16);
  $lo = $b % 16;
  return substr($HEX, $hi, $hi + 1 - $hi) . substr($HEX, $lo, $lo + 1 - $lo);
};
  function bytes_to_hex($bs) {
  global $HEX;
  $res = '';
  $i = 0;
  while ($i < count($bs)) {
  $res = $res . byte_to_hex($bs[$i]);
  $i = $i + 1;
};
  return $res;
};
  echo rtrim(bytes_to_hex(_sha256('Python'))), PHP_EOL;
  echo rtrim(bytes_to_hex(_sha256('hello world'))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
