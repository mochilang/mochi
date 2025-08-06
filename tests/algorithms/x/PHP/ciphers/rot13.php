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
  $uppercase = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lowercase = 'abcdefghijklmnopqrstuvwxyz';
  function index_of($s, $c) {
  global $uppercase, $lowercase;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $c) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function dencrypt($s, $n) {
  global $uppercase, $lowercase;
  $out = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  $idx_u = index_of($uppercase, $ch);
  if ($idx_u >= 0) {
  $new_idx = ($idx_u + $n) % 26;
  $out = $out . substr($uppercase, $new_idx, $new_idx + 1 - $new_idx);
} else {
  $idx_l = index_of($lowercase, $ch);
  if ($idx_l >= 0) {
  $new_idx = ($idx_l + $n) % 26;
  $out = $out . substr($lowercase, $new_idx, $new_idx + 1 - $new_idx);
} else {
  $out = $out . $ch;
};
}
  $i = $i + 1;
};
  return $out;
};
  function main() {
  global $uppercase, $lowercase;
  $msg = 'My secret bank account number is 173-52946 so don\'t tell anyone!!';
  $s = dencrypt($msg, 13);
  echo rtrim($s), PHP_EOL;
  echo rtrim(_str(dencrypt($s, 13) == $msg)), PHP_EOL;
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
