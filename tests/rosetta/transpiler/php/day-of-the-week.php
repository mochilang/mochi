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
function weekday($y, $m, $d) {
  $yy = $y;
  $mm = $m;
  if ($mm < 3) {
  $mm = $mm + 12;
  $yy = $yy - 1;
}
  $k = $yy % 100;
  $j = intval((_intdiv($yy, 100)));
  $a = intval((_intdiv((13 * ($mm + 1)), 5)));
  $b = intval((_intdiv($k, 4)));
  $c = intval((_intdiv($j, 4)));
  return ($d + $a + $k + $b + $c + 5 * $j) % 7;
}
for ($year = 2008; $year < 2122; $year++) {
  if (weekday($year, 12, 25) == 1) {
  echo rtrim('25 December ' . _str($year) . ' is Sunday'), PHP_EOL;
}
}
