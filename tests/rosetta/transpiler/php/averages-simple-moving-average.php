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
  function fmt3($x) {
  $y = floatval(intval((($x * 1000.0) + 0.5))) / 1000.0;
  $s = _str($y);
  $dot = _indexof($s, '.');
  if ($dot == 0 - 1) {
  $s = $s . '.000';
} else {
  $decs = strlen($s) - $dot - 1;
  if ($decs > 3) {
  $s = substr($s, 0, $dot + 4 - 0);
} else {
  while ($decs < 3) {
  $s = $s . '0';
  $decs = $decs + 1;
};
};
}
  return $s;
};
  function pad($s, $width) {
  $out = $s;
  while (strlen($out) < $width) {
  $out = ' ' . $out;
};
  return $out;
};
  function smaSeries($xs, $period) {
  $res = [];
  $sum = 0.0;
  $i = 0;
  while ($i < count($xs)) {
  $sum = $sum + $xs[$i];
  if ($i >= $period) {
  $sum = $sum - $xs[$i - $period];
}
  $denom = $i + 1;
  if ($denom > $period) {
  $denom = $period;
}
  $res = array_merge($res, [$sum / (floatval($denom))]);
  $i = $i + 1;
};
  return $res;
};
  function main() {
  $xs = [1.0, 2.0, 3.0, 4.0, 5.0, 5.0, 4.0, 3.0, 2.0, 1.0];
  $sma3 = smaSeries($xs, 3);
  $sma5 = smaSeries($xs, 5);
  echo rtrim('x       sma3   sma5'), PHP_EOL;
  $i = 0;
  while ($i < count($xs)) {
  $line = pad(fmt3($xs[$i]), 5) . '  ' . pad(fmt3($sma3[$i]), 5) . '  ' . pad(fmt3($sma5[$i]), 5);
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
