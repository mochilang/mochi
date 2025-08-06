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
  function bin_to_octal($bin_string) {
  $i = 0;
  while ($i < strlen($bin_string)) {
  $c = substr($bin_string, $i, $i + 1 - $i);
  if (!($c == '0' || $c == '1')) {
  $panic('Non-binary value was passed to the function');
}
  $i = $i + 1;
};
  if (strlen($bin_string) == 0) {
  $panic('Empty string was passed to the function');
}
  $padded = $bin_string;
  while (fmod(strlen($padded), 3) != 0) {
  $padded = '0' . $padded;
};
  $oct_string = '';
  $index = 0;
  while ($index < strlen($padded)) {
  $group = substr($padded, $index, $index + 3 - $index);
  $b0 = (substr($group, 0, 0 + 1 - 0) == '1' ? 1 : 0);
  $b1 = (substr($group, 1, 1 + 1 - 1) == '1' ? 1 : 0);
  $b2 = (substr($group, 2, 2 + 1 - 2) == '1' ? 1 : 0);
  $oct_val = $b0 * 4 + $b1 * 2 + $b2;
  $oct_string = $oct_string . _str($oct_val);
  $index = $index + 3;
};
  return $oct_string;
};
  echo rtrim(bin_to_octal('1111')), PHP_EOL;
  echo rtrim(bin_to_octal('101010101010011')), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
