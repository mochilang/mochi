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
  function panic($msg) {
  echo rtrim($msg), PHP_EOL;
};
  function powf($base, $exp) {
  $result = 1.0;
  $i = 0;
  while ($i < intval($exp)) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function simple_interest($principal, $daily_rate, $days) {
  if ($days <= 0.0) {
  panic('days_between_payments must be > 0');
  return 0.0;
}
  if ($daily_rate < 0.0) {
  panic('daily_interest_rate must be >= 0');
  return 0.0;
}
  if ($principal <= 0.0) {
  panic('principal must be > 0');
  return 0.0;
}
  return $principal * $daily_rate * $days;
};
  function compound_interest($principal, $nominal_rate, $periods) {
  if ($periods <= 0.0) {
  panic('number_of_compounding_periods must be > 0');
  return 0.0;
}
  if ($nominal_rate < 0.0) {
  panic('nominal_annual_interest_rate_percentage must be >= 0');
  return 0.0;
}
  if ($principal <= 0.0) {
  panic('principal must be > 0');
  return 0.0;
}
  return $principal * (powf(1.0 + $nominal_rate, $periods) - 1.0);
};
  function apr_interest($principal, $apr, $years) {
  if ($years <= 0.0) {
  panic('number_of_years must be > 0');
  return 0.0;
}
  if ($apr < 0.0) {
  panic('nominal_annual_percentage_rate must be >= 0');
  return 0.0;
}
  if ($principal <= 0.0) {
  panic('principal must be > 0');
  return 0.0;
}
  return compound_interest($principal, $apr / 365.0, $years * 365.0);
};
  function main() {
  echo rtrim(_str(simple_interest(18000.0, 0.06, 3.0))), PHP_EOL;
  echo rtrim(_str(simple_interest(0.5, 0.06, 3.0))), PHP_EOL;
  echo rtrim(_str(simple_interest(18000.0, 0.01, 10.0))), PHP_EOL;
  echo rtrim(_str(compound_interest(10000.0, 0.05, 3.0))), PHP_EOL;
  echo rtrim(_str(compound_interest(10000.0, 0.05, 1.0))), PHP_EOL;
  echo rtrim(_str(apr_interest(10000.0, 0.05, 3.0))), PHP_EOL;
  echo rtrim(_str(apr_interest(10000.0, 0.05, 1.0))), PHP_EOL;
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
