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
  $ENERGY_CONVERSION = ['joule' => 1.0, 'kilojoule' => 1000.0, 'megajoule' => 1000000.0, 'gigajoule' => 1000000000.0, 'wattsecond' => 1.0, 'watthour' => 3600.0, 'kilowatthour' => 3600000.0, 'newtonmeter' => 1.0, 'calorie_nutr' => 4186.8, 'kilocalorie_nutr' => 4186800.0, 'electronvolt' => 0.0000000000000000001602176634, 'britishthermalunit_it' => 1055.05585, 'footpound' => 1.355818];
  function energy_conversion($from_type, $to_type, $value) {
  global $ENERGY_CONVERSION;
  if ((array_key_exists($from_type, $ENERGY_CONVERSION)) == false || (array_key_exists($to_type, $ENERGY_CONVERSION)) == false) {
  $panic('Incorrect \'from_type\' or \'to_type\'');
}
  return $value * $ENERGY_CONVERSION[$from_type] / $ENERGY_CONVERSION[$to_type];
};
  echo rtrim(_str(energy_conversion('joule', 'kilojoule', 1.0))), PHP_EOL;
  echo rtrim(_str(energy_conversion('kilowatthour', 'joule', 10.0))), PHP_EOL;
  echo rtrim(_str(energy_conversion('britishthermalunit_it', 'footpound', 1.0))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
