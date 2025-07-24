<?php
ini_set('memory_limit', '-1');
$PI = 3.141592653589793;
$TWO_PI = 6.283185307179586;
function sinApprox($x) {
  global $PI, $TWO_PI, $mochi_floor, $absFloat, $absInt, $parseIntStr, $parseDate, $leap, $daysInMonth, $addDays, $pad2, $dateString, $day, $biorhythms, $main;
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
}
function mochi_floor($x) {
  global $PI, $TWO_PI, $sinApprox, $absFloat, $absInt, $parseIntStr, $parseDate, $leap, $daysInMonth, $addDays, $pad2, $dateString, $day, $biorhythms, $main;
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
}
function absFloat($x) {
  global $PI, $TWO_PI, $sinApprox, $mochi_floor, $absInt, $parseIntStr, $parseDate, $leap, $daysInMonth, $addDays, $pad2, $dateString, $day, $biorhythms, $main;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
}
function absInt($n) {
  global $PI, $TWO_PI, $sinApprox, $mochi_floor, $absFloat, $parseIntStr, $parseDate, $leap, $daysInMonth, $addDays, $pad2, $dateString, $day, $biorhythms, $main;
  if ($n < 0) {
  return -$n;
}
  return $n;
}
function parseIntStr($str) {
  global $PI, $TWO_PI, $sinApprox, $mochi_floor, $absFloat, $absInt, $parseDate, $leap, $daysInMonth, $addDays, $pad2, $dateString, $day, $biorhythms, $main;
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
}
function parseDate($s) {
  global $PI, $TWO_PI, $sinApprox, $mochi_floor, $absFloat, $absInt, $parseIntStr, $leap, $daysInMonth, $addDays, $pad2, $dateString, $day, $biorhythms, $main;
  $y = parseIntStr(substr($s, 0, 4 - 0));
  $m = parseIntStr(substr($s, 5, 7 - 5));
  $d = parseIntStr(substr($s, 8, 10 - 8));
  return [$y, $m, $d];
}
function leap($y) {
  global $PI, $TWO_PI, $sinApprox, $mochi_floor, $absFloat, $absInt, $parseIntStr, $parseDate, $daysInMonth, $addDays, $pad2, $dateString, $day, $biorhythms, $main;
  if ($y % 400 == 0) {
  return true;
}
  if ($y % 100 == 0) {
  return false;
}
  return $y % 4 == 0;
}
function daysInMonth($y, $m) {
  global $PI, $TWO_PI, $sinApprox, $mochi_floor, $absFloat, $absInt, $parseIntStr, $parseDate, $leap, $addDays, $pad2, $dateString, $day, $biorhythms, $main;
  $feb = (leap($y) ? 29 : 28);
  $lengths = [31, $feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  return $lengths[$m - 1];
}
function addDays($y, $m, $d, $n) {
  global $PI, $TWO_PI, $sinApprox, $mochi_floor, $absFloat, $absInt, $parseIntStr, $parseDate, $leap, $daysInMonth, $pad2, $dateString, $day, $biorhythms, $main;
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
}
function pad2($n) {
  global $PI, $TWO_PI, $sinApprox, $mochi_floor, $absFloat, $absInt, $parseIntStr, $parseDate, $leap, $daysInMonth, $addDays, $dateString, $day, $biorhythms, $main;
  if ($n < 10) {
  return '0' . json_encode($n, 1344);
}
  return json_encode($n, 1344);
}
function dateString($y, $m, $d) {
  global $PI, $TWO_PI, $sinApprox, $mochi_floor, $absFloat, $absInt, $parseIntStr, $parseDate, $leap, $daysInMonth, $addDays, $pad2, $day, $biorhythms, $main;
  return json_encode($y, 1344) . '-' . pad2($m) . '-' . pad2($d);
}
function day($y, $m, $d) {
  global $PI, $TWO_PI, $sinApprox, $mochi_floor, $absFloat, $absInt, $parseIntStr, $parseDate, $leap, $daysInMonth, $addDays, $pad2, $dateString, $biorhythms, $main;
  $part1 = 367 * $y;
  $part2 = intval(((7 * (intval(($y + (intdiv(($m + 9), 12)))))) / 4));
  $part3 = intval((intdiv((275 * $m), 9)));
  return $part1 - $part2 + $part3 + $d - 730530;
}
function biorhythms($birth, $target) {
  global $PI, $TWO_PI, $sinApprox, $mochi_floor, $absFloat, $absInt, $parseIntStr, $parseDate, $leap, $daysInMonth, $addDays, $pad2, $dateString, $day, $main;
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
  echo rtrim('Day ' . json_encode($diff, 1344)), PHP_EOL;
  $cycles = ['Physical day ', 'Emotional day', 'Mental day   '];
  $lengths = [23, 28, 33];
  $quadrants = [['up and rising', 'peak'], ['up but falling', 'transition'], ['down and falling', 'valley'], ['down but rising', 'transition']];
  $i = 0;
  while ($i < 3) {
  $length = $lengths[$i];
  $cycle = $cycles[$i];
  $position = $diff % $length;
  $quadrant = intdiv(($position * 4), $length);
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
  $daysToAdd = intdiv(($quadrant + 1) * $length, 4) - $position;
  $res = addDays($ty, $tm, $td, $daysToAdd);
  $ny = $res[0];
  $nm = $res[1];
  $nd = $res[2];
  $transition = dateString($ny, $nm, $nd);
  $trend = $quadrants[$quadrant][0];
  $next = $quadrants[$quadrant][1];
  $pct = json_encode($percent, 1344);
  if (!str_contains($pct, '.')) {
  $pct = $pct . '.0';
};
  $description = ' ' . $pct . '% (' . $trend . ', next ' . $next . ' ' . $transition . ')';
};
};
}
  $posStr = json_encode($position, 1344);
  if ($position < 10) {
  $posStr = ' ' . $posStr;
}
  echo rtrim($cycle . $posStr . ' : ' . $description), PHP_EOL;
  $i = $i + 1;
};
  echo rtrim(''), PHP_EOL;
}
function main() {
  global $PI, $TWO_PI, $sinApprox, $mochi_floor, $absFloat, $absInt, $parseIntStr, $parseDate, $leap, $daysInMonth, $addDays, $pad2, $dateString, $day, $biorhythms;
  $pairs = [['1943-03-09', '1972-07-11'], ['1809-01-12', '1863-11-19'], ['1809-02-12', '1863-11-19']];
  $idx = 0;
  while ($idx < count($pairs)) {
  $p = $pairs[$idx];
  biorhythms($p[0], $p[1]);
  $idx = $idx + 1;
};
}
main();
