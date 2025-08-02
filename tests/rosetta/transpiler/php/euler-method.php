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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function indexOf($s, $ch) {
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function floorf($x) {
  $y = intval($x);
  return floatval($y);
};
  function powf($base, $exp) {
  $r = 1.0;
  $i = 0;
  while ($i < $exp) {
  $r = $r * $base;
  $i = $i + 1;
};
  return $r;
};
  function fmtF($x, $width, $prec) {
  $factor = powf(10.0, $prec);
  $y = floorf($x * $factor + 0.5) / $factor;
  $s = _str($y);
  $dot = _indexof($s, '.');
  if ($dot == 0 - 1) {
  $s = $s . '.';
  $j = 0;
  while ($j < $prec) {
  $s = $s . '0';
  $j = $j + 1;
};
} else {
  $decs = strlen($s) - $dot - 1;
  while ($decs < $prec) {
  $s = $s . '0';
  $decs = $decs + 1;
};
}
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
};
  function expf($x) {
  if ($x < 0.0) {
  return 1.0 / expf(-$x);
}
  $term = 1.0;
  $sum = 1.0;
  $i = 1;
  while ($i < 20) {
  $term = $term * $x / (floatval($i));
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
};
  function eulerStep($f, $x, $y, $h) {
  return $y + $h * $f($x, $y);
};
  function newCoolingRate($k) {
  return function($dt) use ($k) {
  return -$k * $dt;
};
};
  function newTempFunc($k, $ambient, $initial) {
  return function($t) use ($ambient, $k, $initial) {
  return $ambient + ($initial - $ambient) * expf(-$k * $t);
};
};
  function newCoolingRateDy($k, $ambient) {
  $cr = newCoolingRate($k);
  return function($_x, $obj) use ($ambient, $cr, $k) {
  return $cr($obj - $ambient);
};
};
  function main() {
  $k = 0.07;
  $tempRoom = 20.0;
  $tempObject = 100.0;
  $fcr = newCoolingRateDy($k, $tempRoom);
  $analytic = newTempFunc($k, $tempRoom, $tempObject);
  foreach ([2.0, 5.0, 10.0] as $step) {
  echo rtrim('Step size = ' . fmtF($step, 0, 1)), PHP_EOL;
  echo rtrim(' Time Euler\'s Analytic'), PHP_EOL;
  $temp = $tempObject;
  $time = 0.0;
  while ($time <= 100.0) {
  $line = fmtF($time, 5, 1) . ' ' . fmtF($temp, 7, 3) . ' ' . fmtF($analytic($time), 7, 3);
  echo rtrim($line), PHP_EOL;
  $temp = eulerStep($fcr, $time, $temp, $step);
  $time = $time + $step;
};
  echo rtrim(''), PHP_EOL;
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
