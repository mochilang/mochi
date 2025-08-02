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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function capitalize($s) {
  global $small, $tens, $illions;
  if (strlen($s) == 0) {
  return $s;
}
  return strtoupper(substr($s, 0, 1 - 0)) . substr($s, 1, strlen($s) - 1);
};
  $small = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen'];
  $tens = ['', '', 'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety'];
  $illions = ['', ' thousand', ' million', ' billion', ' trillion', ' quadrillion', ' quintillion'];
  function say($n) {
  global $small, $tens, $illions;
  $t = '';
  if ($n < 0) {
  $t = 'negative ';
  $n = -$n;
}
  if ($n < 20) {
  return $t . $small[$n];
} else {
  if ($n < 100) {
  $t = $tens[_intdiv($n, 10)];
  $s = $n % 10;
  if ($s > 0) {
  $t = $t . '-' . $small[$s];
};
  return $t;
} else {
  if ($n < 1000) {
  $t = $small[_intdiv($n, 100)] . ' hundred';
  $s = $n % 100;
  if ($s > 0) {
  $t = $t . ' ' . say($s);
};
  return $t;
};
};
}
  $sx = '';
  $i = 0;
  $nn = $n;
  while ($nn > 0) {
  $p = $nn % 1000;
  $nn = _intdiv($nn, 1000);
  if ($p > 0) {
  $ix = say($p) . $illions[$i];
  if ($sx != '') {
  $ix = $ix . ' ' . $sx;
};
  $sx = $ix;
}
  $i = $i + 1;
};
  return $t . $sx;
};
  function fourIsMagic($n) {
  global $small, $tens, $illions;
  $s = say($n);
  $s = capitalize($s);
  $t = $s;
  while ($n != 4) {
  $n = strlen($s);
  $s = say($n);
  $t = $t . ' is ' . $s . ', ' . $s;
};
  $t = $t . ' is magic.';
  return $t;
};
  function main() {
  global $small, $tens, $illions;
  $nums = [0, 4, 6, 11, 13, 75, 100, 337, -164, 9223372036854775807];
  foreach ($nums as $n) {
  echo rtrim(fourIsMagic($n)), PHP_EOL;
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
