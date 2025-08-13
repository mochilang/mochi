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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_sqrt($x) {
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 10) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function real_power($apparent_power, $power_factor) {
  if ($power_factor < 0.0 - 1.0 || $power_factor > 1.0) {
  _panic('power_factor must be a valid float value between -1 and 1.');
}
  return $apparent_power * $power_factor;
};
  function reactive_power($apparent_power, $power_factor) {
  if ($power_factor < 0.0 - 1.0 || $power_factor > 1.0) {
  _panic('power_factor must be a valid float value between -1 and 1.');
}
  return $apparent_power * mochi_sqrt(1.0 - $power_factor * $power_factor);
};
  echo rtrim(_str(real_power(100.0, 0.9))), PHP_EOL;
  echo rtrim(_str(real_power(0.0, 0.8))), PHP_EOL;
  echo rtrim(_str(real_power(100.0, -0.9))), PHP_EOL;
  echo rtrim(_str(reactive_power(100.0, 0.9))), PHP_EOL;
  echo rtrim(_str(reactive_power(0.0, 0.8))), PHP_EOL;
  echo rtrim(_str(reactive_power(100.0, -0.9))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
