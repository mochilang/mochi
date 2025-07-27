<?php
ini_set('memory_limit', '-1');
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
$dayNames = ['Sweetmorn', 'Boomtime', 'Pungenday', 'Prickle-Prickle', 'Setting Orange'];
$seasons = ['Chaos', 'Discord', 'Confusion', 'Bureaucracy', 'The Aftermath'];
$holydays = [['Mungday', 'Chaoflux'], ['Mojoday', 'Discoflux'], ['Syaday', 'Confuflux'], ['Zaraday', 'Bureflux'], ['Maladay', 'Afflux']];
function isLeap($y) {
  global $dayNames, $seasons, $holydays, $daysBefore;
  if ($y % 400 == 0) {
  return true;
}
  if ($y % 100 == 0) {
  return false;
}
  return $y % 4 == 0;
}
$daysBefore = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];
function dayOfYear($y, $m, $d) {
  global $dayNames, $seasons, $holydays, $daysBefore;
  $doy = $daysBefore[$m - 1] + $d;
  if ($m > 2 && isLeap($y)) {
  $doy = $doy + 1;
}
  return $doy;
}
function ordinal($n) {
  global $dayNames, $seasons, $holydays, $daysBefore;
  $suff = 'th';
  $mod100 = $n % 100;
  if ($mod100 < 11 || $mod100 > 13) {
  $r = $n % 10;
  if ($r == 1) {
  $suff = 'st';
} else {
  if ($r == 2) {
  $suff = 'nd';
} else {
  if ($r == 3) {
  $suff = 'rd';
};
};
};
}
  return _str($n) . $suff;
}
function discordian($y, $m, $d) {
  global $dayNames, $seasons, $holydays, $daysBefore;
  if (isLeap($y) && $m == 2 && $d == 29) {
  return 'St. Tib\'s Day, YOLD ' . _str($y + 1166);
}
  $doy = dayOfYear($y, $m, $d);
  if (isLeap($y) && $doy > 60) {
  $doy = $doy - 1;
}
  $idx = $doy - 1;
  $season = _intdiv($idx, 73);
  $day = $idx % 73;
  $res = $dayNames[$idx % 5] . ', the ' . ordinal($day + 1) . ' day of ' . $seasons[$season] . ' in the YOLD ' . _str($y + 1166);
  if ($day == 4) {
  $res = $res . '. Celebrate ' . $holydays[$season][0] . '!';
}
  if ($day == 49) {
  $res = $res . '. Celebrate ' . $holydays[$season][1] . '!';
}
  return $res;
}
function main() {
  global $dayNames, $seasons, $holydays, $daysBefore;
  $dates = [[2010, 7, 22], [2012, 2, 28], [2012, 2, 29], [2012, 3, 1], [2012, 12, 31], [2013, 1, 1], [2100, 12, 31], [2015, 10, 19], [2010, 1, 5], [2011, 5, 3], [2000, 3, 13]];
  $i = 0;
  while ($i < count($dates)) {
  $dt = $dates[$i];
  echo rtrim(discordian($dt[0], $dt[1], $dt[2])), PHP_EOL;
  $i = $i + 1;
};
}
main();
