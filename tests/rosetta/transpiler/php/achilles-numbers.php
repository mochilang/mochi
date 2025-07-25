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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function pow10($exp) {
  global $pps;
  $n = 1;
  $i = 0;
  while ($i < $exp) {
  $n = $n * 10;
  $i = $i + 1;
};
  return $n;
};
  function totient($n) {
  global $pps;
  $tot = $n;
  $nn = $n;
  $i = 2;
  while ($i * $i <= $nn) {
  if ($nn % $i == 0) {
  while ($nn % $i == 0) {
  $nn = _intdiv($nn, $i);
};
  $tot = $tot - _intdiv($tot, $i);
}
  if ($i == 2) {
  $i = 1;
}
  $i = $i + 2;
};
  if ($nn > 1) {
  $tot = $tot - _intdiv($tot, $nn);
}
  return $tot;
};
  $pps = [];
  function getPerfectPowers($maxExp) {
  global $pps;
  $upper = pow10($maxExp);
  $i = 2;
  while ($i * $i < $upper) {
  $p = $i;
  while (true) {
  $p = $p * $i;
  if ($p >= $upper) {
  break;
}
  $pps[$p] = true;
};
  $i = $i + 1;
};
};
  function getAchilles($minExp, $maxExp) {
  global $pps;
  $lower = pow10($minExp);
  $upper = pow10($maxExp);
  $achilles = [];
  $b = 1;
  while ($b * $b * $b < $upper) {
  $b3 = $b * $b * $b;
  $a = 1;
  while (true) {
  $p = $b3 * $a * $a;
  if ($p >= $upper) {
  break;
}
  if ($p >= $lower) {
  if (!(array_key_exists($p, $pps))) {
  $achilles[$p] = true;
};
}
  $a = $a + 1;
};
  $b = $b + 1;
};
  return $achilles;
};
  function sortInts($xs) {
  global $pps;
  $res = [];
  $tmp = $xs;
  while (count($tmp) > 0) {
  $min = $tmp[0];
  $idx = 0;
  $i = 1;
  while ($i < count($tmp)) {
  if ($tmp[$i] < $min) {
  $min = $tmp[$i];
  $idx = $i;
}
  $i = $i + 1;
};
  $res = array_merge($res, [$min]);
  $out = [];
  $j = 0;
  while ($j < count($tmp)) {
  if ($j != $idx) {
  $out = array_merge($out, [$tmp[$j]]);
}
  $j = $j + 1;
};
  $tmp = $out;
};
  return $res;
};
  function pad($n, $width) {
  global $pps;
  $s = _str($n);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
};
  function main() {
  global $pps;
  $maxDigits = 15;
  getPerfectPowers(5);
  $achSet = getAchilles(1, 5);
  $ach = [];
  foreach (array_keys($achSet) as $k) {
  $ach = array_merge($ach, [$k]);
};
  $ach = sortInts($ach);
  echo rtrim('First 50 Achilles numbers:'), PHP_EOL;
  $i = 0;
  while ($i < 50) {
  $line = '';
  $j = 0;
  while ($j < 10) {
  $line = $line . pad($ach[$i], 4);
  if ($j < 9) {
  $line = $line . ' ';
}
  $i = $i + 1;
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
};
  echo rtrim('\nFirst 30 strong Achilles numbers:'), PHP_EOL;
  $strong = [];
  $count = 0;
  $idx = 0;
  while ($count < 30) {
  $tot = totient($ach[$idx]);
  if (array_key_exists($tot, $achSet)) {
  $strong = array_merge($strong, [$ach[$idx]]);
  $count = $count + 1;
}
  $idx = $idx + 1;
};
  $i = 0;
  while ($i < 30) {
  $line = '';
  $j = 0;
  while ($j < 10) {
  $line = $line . pad($strong[$i], 5);
  if ($j < 9) {
  $line = $line . ' ';
}
  $i = $i + 1;
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
};
  echo rtrim('\nNumber of Achilles numbers with:'), PHP_EOL;
  $counts = [1, 12, 47, 192, 664, 2242, 7395, 24008, 77330, 247449, 788855, 2508051, 7960336, 25235383];
  $d = 2;
  while ($d <= $maxDigits) {
  $c = $counts[$d - 2];
  echo rtrim(pad($d, 2) . ' digits: ' . _str($c)), PHP_EOL;
  $d = $d + 1;
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
