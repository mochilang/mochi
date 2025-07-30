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
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $gregorianStr = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];
  $gregorian = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  $republicanStr = ['Vendemiaire', 'Brumaire', 'Frimaire', 'Nivose', 'Pluviose', 'Ventose', 'Germinal', 'Floreal', 'Prairial', 'Messidor', 'Thermidor', 'Fructidor'];
  $sansculotidesStr = ['Fete de la vertu', 'Fete du genie', 'Fete du travail', 'Fete de l\'opinion', 'Fete des recompenses', 'Fete de la Revolution'];
  function greLeap($year) {
  global $gregorianStr, $gregorian, $republicanStr, $sansculotidesStr, $rep, $gre;
  $a = intval(($year % 4));
  $b = intval(($year % 100));
  $c = intval(($year % 400));
  return $a == 0 && ($b != 0 || $c == 0);
};
  function repLeap($year) {
  global $gregorianStr, $gregorian, $republicanStr, $sansculotidesStr, $rep, $gre;
  $a = intval((($year + 1) % 4));
  $b = intval((($year + 1) % 100));
  $c = intval((($year + 1) % 400));
  return $a == 0 && ($b != 0 || $c == 0);
};
  function greToDay($d, $m, $y) {
  global $gregorianStr, $gregorian, $republicanStr, $sansculotidesStr, $rep, $gre;
  $yy = $y;
  $mm = $m;
  if ($mm < 3) {
  $yy = $yy - 1;
  $mm = $mm + 12;
}
  return _intdiv($yy * 36525, 100) - _intdiv($yy, 100) + _intdiv($yy, 400) + _intdiv(306 * ($mm + 1), 10) + $d - 654842;
};
  function repToDay($d, $m, $y) {
  global $gregorianStr, $gregorian, $republicanStr, $sansculotidesStr, $rep, $gre;
  $dd = $d;
  $mm = $m;
  if ($mm == 13) {
  $mm = $mm - 1;
  $dd = $dd + 30;
}
  if (repLeap($y)) {
  $dd = $dd - 1;
}
  return 365 * $y + _intdiv(($y + 1), 4) - _intdiv(($y + 1), 100) + _intdiv(($y + 1), 400) + 30 * $mm + $dd - 395;
};
  function dayToGre($day) {
  global $gregorianStr, $gregorian, $republicanStr, $sansculotidesStr, $rep, $gre;
  $y = _intdiv($day * 100, 36525);
  $d = $day - _intdiv($y * 36525, 100) + 21;
  $y = $y + 1792;
  $d = $d + _intdiv($y, 100) - _intdiv($y, 400) - 13;
  $m = 8;
  while ($d > $gregorian[$m]) {
  $d = $d - $gregorian[$m];
  $m = $m + 1;
  if ($m == 12) {
  $m = 0;
  $y = $y + 1;
  if (greLeap($y)) {
  $gregorian[1] = 29;
} else {
  $gregorian[1] = 28;
};
}
};
  $m = $m + 1;
  return [$d, $m, $y];
};
  function dayToRep($day) {
  global $gregorianStr, $gregorian, $republicanStr, $sansculotidesStr, $rep, $gre;
  $y = _intdiv(($day - 1) * 100, 36525);
  if (repLeap($y)) {
  $y = $y - 1;
}
  $d = $day - _intdiv(($y + 1) * 36525, 100) + 365 + _intdiv(($y + 1), 100) - _intdiv(($y + 1), 400);
  $y = $y + 1;
  $m = 1;
  $sc = 5;
  if (repLeap($y)) {
  $sc = 6;
}
  while ($d > 30) {
  $d = $d - 30;
  $m = $m + 1;
  if ($m == 13) {
  if ($d > $sc) {
  $d = $d - $sc;
  $m = 1;
  $y = $y + 1;
  $sc = 5;
  if (repLeap($y)) {
  $sc = 6;
};
};
}
};
  return [$d, $m, $y];
};
  function formatRep($d, $m, $y) {
  global $gregorianStr, $gregorian, $republicanStr, $sansculotidesStr, $rep, $gre;
  if ($m == 13) {
  return $sansculotidesStr[$d - 1] . ' ' . _str($y);
}
  return _str($d) . ' ' . $republicanStr[$m - 1] . ' ' . _str($y);
};
  function formatGre($d, $m, $y) {
  global $gregorianStr, $gregorian, $republicanStr, $sansculotidesStr, $rep, $gre;
  return _str($d) . ' ' . $gregorianStr[$m - 1] . ' ' . _str($y);
};
  $rep = dayToRep(greToDay(20, 5, 1795));
  echo rtrim(formatRep($rep[0], $rep[1], $rep[2])), PHP_EOL;
  $gre = dayToGre(repToDay(1, 9, 3));
  echo rtrim(formatGre($gre[0], $gre[1], $gre[2])), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
