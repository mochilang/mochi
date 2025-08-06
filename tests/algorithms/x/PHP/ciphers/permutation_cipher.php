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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $seed = 1;
  function mochi_rand($max) {
  global $seed, $message, $block_size, $key, $encrypted, $decrypted;
  $seed = ($seed * 1103515245 + 12345) % 2147483647;
  return $seed % $max;
};
  function generate_valid_block_size($message_length) {
  global $seed, $message, $block_size, $key, $encrypted, $decrypted;
  $factors = [];
  $i = 2;
  while ($i <= $message_length) {
  if ($message_length % $i == 0) {
  $factors = _append($factors, $i);
}
  $i = $i + 1;
};
  $idx = mochi_rand(count($factors));
  return $factors[$idx];
};
  function generate_permutation_key($block_size) {
  global $seed, $message, $key, $encrypted, $decrypted;
  $digits = [];
  $i = 0;
  while ($i < $block_size) {
  $digits = _append($digits, $i);
  $i = $i + 1;
};
  $j = $block_size - 1;
  while ($j > 0) {
  $k = mochi_rand($j + 1);
  $temp = $digits[$j];
  $digits[$j] = $digits[$k];
  $digits[$k] = $temp;
  $j = $j - 1;
};
  return $digits;
};
  function encrypt($message, $key, $block_size) {
  global $seed, $decrypted;
  $encrypted = '';
  $i = 0;
  while ($i < strlen($message)) {
  $block = substr($message, $i, $i + $block_size - $i);
  $j = 0;
  while ($j < $block_size) {
  $encrypted = $encrypted . substr($block, $key[$j], $key[$j] + 1 - $key[$j]);
  $j = $j + 1;
};
  $i = $i + $block_size;
};
  return $encrypted;
};
  function repeat_string($times) {
  global $seed, $message, $block_size, $key, $encrypted, $decrypted;
  $res = [];
  $i = 0;
  while ($i < $times) {
  $res = _append($res, '');
  $i = $i + 1;
};
  return $res;
};
  function decrypt($encrypted, $key) {
  global $seed, $message, $block_size;
  $klen = count($key);
  $decrypted = '';
  $i = 0;
  while ($i < strlen($encrypted)) {
  $block = substr($encrypted, $i, $i + $klen - $i);
  $original = repeat_string($klen);
  $j = 0;
  while ($j < $klen) {
  $original[$key[$j]] = substr($block, $j, $j + 1 - $j);
  $j = $j + 1;
};
  $j = 0;
  while ($j < $klen) {
  $decrypted = $decrypted . $original[$j];
  $j = $j + 1;
};
  $i = $i + $klen;
};
  return $decrypted;
};
  $message = 'HELLO WORLD';
  $block_size = generate_valid_block_size(strlen($message));
  $key = generate_permutation_key($block_size);
  $encrypted = encrypt($message, $key, $block_size);
  $decrypted = decrypt($encrypted, $key);
  echo rtrim('Block size: ' . _str($block_size)), PHP_EOL;
  echo rtrim('Key: ' . _str($key)), PHP_EOL;
  echo rtrim('Encrypted: ' . $encrypted), PHP_EOL;
  echo rtrim('Decrypted: ' . $decrypted), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
