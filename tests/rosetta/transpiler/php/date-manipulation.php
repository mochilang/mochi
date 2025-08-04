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
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
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
  $months = ['January' => 1, 'February' => 2, 'March' => 3, 'April' => 4, 'May' => 5, 'June' => 6, 'July' => 7, 'August' => 8, 'September' => 9, 'October' => 10, 'November' => 11, 'December' => 12];
  function isLeap($y) {
  global $months;
  if ($y % 400 == 0) {
  return true;
}
  if ($y % 100 == 0) {
  return false;
}
  return $y % 4 == 0;
};
  function daysInMonth($y, $m) {
  global $months;
  $feb = (isLeap($y) ? 29 : 28);
  $lengths = [31, $feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  return $lengths[$m - 1];
};
  function daysBeforeYear($y) {
  global $months;
  $days = 0;
  $yy = 1970;
  while ($yy < $y) {
  $days = $days + 365;
  if (isLeap($yy)) {
  $days = $days + 1;
}
  $yy = $yy + 1;
};
  return $days;
};
  function daysBeforeMonth($y, $m) {
  global $months;
  $days = 0;
  $mm = 1;
  while ($mm < $m) {
  $days = $days + daysInMonth($y, $mm);
  $mm = $mm + 1;
};
  return $days;
};
  function epochSeconds($y, $m, $d, $h, $mi) {
  global $months;
  $days = daysBeforeYear($y) + daysBeforeMonth($y, $m) + ($d - 1);
  return $days * 86400 + $h * 3600 + $mi * 60;
};
  function fromEpoch($sec) {
  global $months;
  $days = _intdiv($sec, 86400);
  $rem = $sec % 86400;
  $y = 1970;
  while (true) {
  $dy = (isLeap($y) ? 366 : 365);
  if ($days >= $dy) {
  $days = $days - $dy;
  $y = $y + 1;
} else {
  break;
}
};
  $m = 1;
  while (true) {
  $dim = daysInMonth($y, $m);
  if ($days >= $dim) {
  $days = $days - $dim;
  $m = $m + 1;
} else {
  break;
}
};
  $d = $days + 1;
  $h = _intdiv($rem, 3600);
  $mi = _intdiv(($rem % 3600), 60);
  return [$y, $m, $d, $h, $mi];
};
  function pad2($n) {
  global $months;
  if ($n < 10) {
  return '0' . _str($n);
}
  return _str($n);
};
  function absInt($n) {
  global $months;
  if ($n < 0) {
  return -$n;
}
  return $n;
};
  function formatDate($parts, $offset, $abbr) {
  global $months;
  $y = $parts[0];
  $m = $parts[1];
  $d = $parts[2];
  $h = $parts[3];
  $mi = $parts[4];
  $sign = '+';
  if ($offset < 0) {
  $sign = '-';
}
  $off = absInt($offset) / 60;
  $offh = pad2(_intdiv($off, 60));
  $offm = pad2($off % 60);
  return _str($y) . '-' . pad2($m) . '-' . pad2($d) . ' ' . pad2($h) . ':' . pad2($mi) . ':00 ' . $sign . $offh . $offm . ' ' . $abbr;
};
  function mochi_parseIntStr($str) {
  global $months;
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
  function indexOf($s, $ch) {
  global $months;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function parseTime($s) {
  global $months;
  $c = _indexof($s, ':');
  $h = parseIntStr(substr($s, 0, $c - 0), 10);
  $mi = parseIntStr(substr($s, $c + 1, $c + 3 - ($c + 1)), 10);
  $ampm = substr($s, strlen($s) - 2, strlen($s) - (strlen($s) - 2));
  $hh = $h;
  if ($ampm == 'pm' && $h != 12) {
  $hh = $h + 12;
}
  if ($ampm == 'am' && $h == 12) {
  $hh = 0;
}
  return [$hh, $mi];
};
  function main() {
  global $months;
  $input = 'March 7 2009 7:30pm EST';
  echo rtrim('Input:              ' . $input), PHP_EOL;
  $parts = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($input)) {
  $ch = substr($input, $i, $i + 1 - $i);
  if ($ch == ' ') {
  if (strlen($cur) > 0) {
  $parts = _append($parts, $cur);
  $cur = '';
};
} else {
  $cur = $cur . $ch;
}
  $i = $i + 1;
};
  if (strlen($cur) > 0) {
  $parts = _append($parts, $cur);
}
  $month = $months[$parts[0]];
  $day = parseIntStr($parts[1], 10);
  $year = parseIntStr($parts[2], 10);
  $tm = parseTime($parts[3]);
  $hour = $tm[0];
  $minute = $tm[1];
  $tz = $parts[4];
  $zoneOffsets = ['EST' => -18000, 'EDT' => -14400, 'MST' => -25200];
  $local = epochSeconds($year, $month, $day, $hour, $minute);
  $utc = $local - $zoneOffsets[$tz];
  $utc12 = $utc + 43200;
  $startDST = epochSeconds(2009, 3, 8, 7, 0);
  $offEast = -18000;
  if ($utc12 >= $startDST) {
  $offEast = -14400;
}
  $eastParts = fromEpoch($utc12 + $offEast);
  $eastAbbr = 'EST';
  if ($offEast == (-14400)) {
  $eastAbbr = 'EDT';
}
  echo rtrim('+12 hrs:            ' . formatDate($eastParts, $offEast, $eastAbbr)), PHP_EOL;
  $offAZ = -25200;
  $azParts = fromEpoch($utc12 + $offAZ);
  echo rtrim('+12 hrs in Arizona: ' . formatDate($azParts, $offAZ, 'MST')), PHP_EOL;
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
