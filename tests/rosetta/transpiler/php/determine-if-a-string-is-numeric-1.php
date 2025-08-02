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
  function isNumeric($s) {
  if ($s == 'NaN') {
  return true;
}
  $i = 0;
  if (strlen($s) == 0) {
  return false;
}
  if (substr($s, 0, 0 + 1 - 0) == '+' || substr($s, 0, 0 + 1 - 0) == '-') {
  if (strlen($s) == 1) {
  return false;
};
  $i = 1;
}
  $digits = false;
  $dot = false;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch >= '0' && $ch <= '9') {
  $digits = true;
  $i = $i + 1;
} else {
  if ($ch == '.' && $dot == false) {
  $dot = true;
  $i = $i + 1;
} else {
  if (($ch == 'e' || $ch == 'E') && $digits) {
  $i = $i + 1;
  if ($i < strlen($s) && (substr($s, $i, $i + 1 - $i) == '+' || substr($s, $i, $i + 1 - $i) == '-')) {
  $i = $i + 1;
};
  $ed = false;
  while ($i < strlen($s) && substr($s, $i, $i + 1 - $i) >= '0' && substr($s, $i, $i + 1 - $i) <= '9') {
  $ed = true;
  $i = $i + 1;
};
  return $ed && $i == strlen($s);
} else {
  return false;
};
};
}
};
  return $digits;
};
  function main() {
  echo rtrim('Are these strings numeric?'), PHP_EOL;
  $strs = ['1', '3.14', '-100', '1e2', 'NaN', 'rose'];
  foreach ($strs as $s) {
  echo rtrim('  ' . $s . ' -> ' . _str(isNumeric($s))), PHP_EOL;
};
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
