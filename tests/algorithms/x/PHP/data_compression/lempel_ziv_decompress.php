<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
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
    return intdiv(intval($a), intval($b));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function list_contains($xs, $v) {
  global $decompressed, $sample;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $v) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function is_power_of_two($n) {
  global $decompressed, $sample;
  if ($n < 1) {
  return false;
}
  $x = $n;
  while ($x > 1) {
  if ($x % 2 != 0) {
  return false;
}
  $x = _intdiv($x, 2);
};
  return true;
};
  function bin_string($n) {
  global $decompressed, $sample;
  if ($n == 0) {
  return '0';
}
  $res = '';
  $x = $n;
  while ($x > 0) {
  $bit = $x % 2;
  $res = _str($bit) . $res;
  $x = _intdiv($x, 2);
};
  return $res;
};
  function decompress_data($data_bits) {
  global $decompressed, $sample;
  $lexicon = ['0' => '0', '1' => '1'];
  $keys = ['0', '1'];
  $result = '';
  $curr_string = '';
  $index = 2;
  $i = 0;
  while ($i < strlen($data_bits)) {
  $curr_string = $curr_string . substr($data_bits, $i, $i + 1 - $i);
  if (!list_contains($keys, $curr_string)) {
  $i = $i + 1;
  continue;
}
  $last_match_id = $lexicon[$curr_string];
  $result = $result . $last_match_id;
  $lexicon[$curr_string] = $last_match_id . '0';
  if (is_power_of_two($index)) {
  $new_lex = [];
  $new_keys = [];
  $j = 0;
  while ($j < count($keys)) {
  $curr_key = $keys[$j];
  $new_lex['0' . $curr_key] = $lexicon[$curr_key];
  $new_keys = _append($new_keys, '0' . $curr_key);
  $j = $j + 1;
};
  $lexicon = $new_lex;
  $keys = $new_keys;
}
  $new_key = bin_string($index);
  $lexicon[$new_key] = $last_match_id . '1';
  $keys = _append($keys, $new_key);
  $index = $index + 1;
  $curr_string = '';
  $i = $i + 1;
};
  return $result;
};
  $sample = '1011001';
  $decompressed = decompress_data($sample);
  echo rtrim($decompressed), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
