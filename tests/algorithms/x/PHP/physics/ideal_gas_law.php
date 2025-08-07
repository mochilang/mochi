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
$__start_mem = memory_get_usage();
$__start = _now();
  $UNIVERSAL_GAS_CONSTANT = 8.314462;
  function pressure_of_gas_system($moles, $kelvin, $volume) {
  global $UNIVERSAL_GAS_CONSTANT;
  if ($moles < 0 || $kelvin < 0 || $volume < 0) {
  $throw;
  'Invalid inputs. Enter positive value.';
}
  return $moles * $kelvin * $UNIVERSAL_GAS_CONSTANT / $volume;
};
  function volume_of_gas_system($moles, $kelvin, $pressure) {
  global $UNIVERSAL_GAS_CONSTANT;
  if ($moles < 0 || $kelvin < 0 || $pressure < 0) {
  $throw;
  'Invalid inputs. Enter positive value.';
}
  return $moles * $kelvin * $UNIVERSAL_GAS_CONSTANT / $pressure;
};
  function temperature_of_gas_system($moles, $volume, $pressure) {
  global $UNIVERSAL_GAS_CONSTANT;
  if ($moles < 0 || $volume < 0 || $pressure < 0) {
  $throw;
  'Invalid inputs. Enter positive value.';
}
  return $pressure * $volume / ($moles * $UNIVERSAL_GAS_CONSTANT);
};
  function moles_of_gas_system($kelvin, $volume, $pressure) {
  global $UNIVERSAL_GAS_CONSTANT;
  if ($kelvin < 0 || $volume < 0 || $pressure < 0) {
  $throw;
  'Invalid inputs. Enter positive value.';
}
  return $pressure * $volume / ($kelvin * $UNIVERSAL_GAS_CONSTANT);
};
  echo rtrim(json_encode(pressure_of_gas_system(2.0, 100.0, 5.0), 1344)), PHP_EOL;
  echo rtrim(json_encode(volume_of_gas_system(0.5, 273.0, 0.004), 1344)), PHP_EOL;
  echo rtrim(json_encode(temperature_of_gas_system(2.0, 100.0, 5.0), 1344)), PHP_EOL;
  echo rtrim(json_encode(moles_of_gas_system(100.0, 5.0, 10.0), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
