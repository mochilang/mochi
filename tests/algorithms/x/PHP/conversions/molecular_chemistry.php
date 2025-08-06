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
  function round_to_int($x) {
  if ($x >= 0.0) {
  return intval($x + 0.5);
}
  return intval($x - 0.5);
};
  function molarity_to_normality($nfactor, $moles, $volume) {
  return round_to_int(($moles / $volume) * $nfactor);
};
  function moles_to_pressure($volume, $moles, $temperature) {
  return round_to_int(($moles * 0.0821 * $temperature) / $volume);
};
  function moles_to_volume($pressure, $moles, $temperature) {
  return round_to_int(($moles * 0.0821 * $temperature) / $pressure);
};
  function pressure_and_volume_to_temperature($pressure, $moles, $volume) {
  return round_to_int(($pressure * $volume) / (0.0821 * $moles));
};
  echo rtrim(_str(molarity_to_normality(2.0, 3.1, 0.31))), PHP_EOL;
  echo rtrim(_str(molarity_to_normality(4.0, 11.4, 5.7))), PHP_EOL;
  echo rtrim(_str(moles_to_pressure(0.82, 3.0, 300.0))), PHP_EOL;
  echo rtrim(_str(moles_to_pressure(8.2, 5.0, 200.0))), PHP_EOL;
  echo rtrim(_str(moles_to_volume(0.82, 3.0, 300.0))), PHP_EOL;
  echo rtrim(_str(moles_to_volume(8.2, 5.0, 200.0))), PHP_EOL;
  echo rtrim(_str(pressure_and_volume_to_temperature(0.82, 1.0, 2.0))), PHP_EOL;
  echo rtrim(_str(pressure_and_volume_to_temperature(8.2, 5.0, 3.0))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
