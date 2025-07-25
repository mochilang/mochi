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
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
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
  $PI = 3.141592653589793;
  $TWO_PI = 6.283185307179586;
  function sinApprox($x) {
  global $PI, $TWO_PI;
  $term = $x;
  $sum = $x;
  $n = 1;
  while ($n <= 8) {
  $denom = floatval(((2 * $n) * (2 * $n + 1)));
  $term = -$term * $x * $x / $denom;
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  function mochi_floor($x) {
  global $PI, $TWO_PI;
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
};
  function absFloat($x) {
  global $PI, $TWO_PI;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function absInt($n) {
  global $PI, $TWO_PI;
  if ($n < 0) {
  return -$n;
}
  return $n;
};
  function mochi_parseIntStr($str) {
  global $PI, $TWO_PI;
  $i = 0;
  $neg = false;
  if (strlen($str) > 0 && substr($str, 0, 1 - 0) == '-') {
  $neg = true;
  $i = 1;
}
  $n = 0;
  $digits = ['0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9];
  while ($i < strlen($str)) {
  $n = $n * 10 + $digits[substr($str, $i, $i + 1 - $i)];
  $i = $i + 1;
};
  if ($neg) {
  $n = -$n;
}
  return $n;
};
  function parseDate($s) {
  global $PI, $TWO_PI;
  $y = parseIntStr(substr($s, 0, 4 - 0), 10);
  $m = parseIntStr(substr($s, 5, 7 - 5), 10);
  $d = parseIntStr(substr($s, 8, 10 - 8), 10);
  return [$y, $m, $d];
};
  function leap($y) {
  global $PI, $TWO_PI;
  if ($y % 400 == 0) {
  return true;
}
  if ($y % 100 == 0) {
  return false;
}
  return $y % 4 == 0;
};
  function daysInMonth($y, $m) {
  global $PI, $TWO_PI;
  $feb = (leap($y) ? 29 : 28);
  $lengths = [31, $feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  return $lengths[$m - 1];
};
  function addDays($y, $m, $d, $n) {
  global $PI, $TWO_PI;
  $yy = $y;
  $mm = $m;
  $dd = $d;
  if ($n >= 0) {
  $i = 0;
  while ($i < $n) {
  $dd = $dd + 1;
  if ($dd > daysInMonth($yy, $mm)) {
  $dd = 1;
  $mm = $mm + 1;
  if ($mm > 12) {
  $mm = 1;
  $yy = $yy + 1;
};
}
  $i = $i + 1;
};
} else {
  $i = 0;
  while ($i > $n) {
  $dd = $dd - 1;
  if ($dd < 1) {
  $mm = $mm - 1;
  if ($mm < 1) {
  $mm = 12;
  $yy = $yy - 1;
};
  $dd = daysInMonth($yy, $mm);
}
  $i = $i - 1;
};
}
  return [$yy, $mm, $dd];
};
  function pad2($n) {
  global $PI, $TWO_PI;
  if ($n < 10) {
  return '0' . _str($n);
}
  return _str($n);
};
  function dateString($y, $m, $d) {
  global $PI, $TWO_PI;
  return _str($y) . '-' . pad2($m) . '-' . pad2($d);
};
  function day($y, $m, $d) {
  global $PI, $TWO_PI;
  $part1 = 367 * $y;
  $part2 = intval(((7 * (intval(($y + (_intdiv(($m + 9), 12)))))) / 4));
  $part3 = intval((_intdiv((275 * $m), 9)));
  return $part1 - $part2 + $part3 + $d - 730530;
};
  function biorhythms($birth, $target) {
  global $PI, $TWO_PI;
  $bparts = parseDate($birth);
  $by = $bparts[0];
  $bm = $bparts[1];
  $bd = $bparts[2];
  $tparts = parseDate($target);
  $ty = $tparts[0];
  $tm = $tparts[1];
  $td = $tparts[2];
  $diff = absInt(day($ty, $tm, $td) - day($by, $bm, $bd));
  echo rtrim('Born ' . $birth . ', Target ' . $target), PHP_EOL;
  echo rtrim('Day ' . _str($diff)), PHP_EOL;
  $cycles = ['Physical day ', 'Emotional day', 'Mental day   '];
  $lengths = [23, 28, 33];
  $quadrants = [['up and rising', 'peak'], ['up but falling', 'transition'], ['down and falling', 'valley'], ['down but rising', 'transition']];
  $i = 0;
  while ($i < 3) {
  $length = $lengths[$i];
  $cycle = $cycles[$i];
  $position = $diff % $length;
  $quadrant = _intdiv(($position * 4), $length);
  $percent = sinApprox(2.0 * $PI * (floatval($position)) / (floatval($length)));
  $percent = mochi_floor($percent * 1000.0) / 10.0;
  $description = '';
  if ($percent > 95.0) {
  $description = ' peak';
} else {
  if ($percent < (-95.0)) {
  $description = ' valley';
} else {
  if (absFloat($percent) < 5.0) {
  $description = ' critical transition';
} else {
  $daysToAdd = _intdiv(($quadrant + 1) * $length, 4) - $position;
  $res = addDays($ty, $tm, $td, $daysToAdd);
  $ny = $res[0];
  $nm = $res[1];
  $nd = $res[2];
  $transition = dateString($ny, $nm, $nd);
  $trend = $quadrants[$quadrant][0];
  $next = $quadrants[$quadrant][1];
  $pct = _str($percent);
  if (!str_contains($pct, '.')) {
  $pct = $pct . '.0';
};
  $description = ' ' . $pct . '% (' . $trend . ', next ' . $next . ' ' . $transition . ')';
};
};
}
  $posStr = _str($position);
  if ($position < 10) {
  $posStr = ' ' . $posStr;
}
  echo rtrim($cycle . $posStr . ' : ' . $description), PHP_EOL;
  $i = $i + 1;
};
  echo rtrim(''), PHP_EOL;
};
  function main() {
  global $PI, $TWO_PI;
  $pairs = [['1943-03-09', '1972-07-11'], ['1809-01-12', '1863-11-19'], ['1809-02-12', '1863-11-19']];
  $idx = 0;
  while ($idx < count($pairs)) {
  $p = $pairs[$idx];
  biorhythms($p[0], $p[1]);
  $idx = $idx + 1;
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
