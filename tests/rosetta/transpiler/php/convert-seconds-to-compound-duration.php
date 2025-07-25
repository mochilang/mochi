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
function timeStr($sec) {
  $wks = _intdiv($sec, 604800);
  $sec = $sec % 604800;
  $ds = _intdiv($sec, 86400);
  $sec = $sec % 86400;
  $hrs = _intdiv($sec, 3600);
  $sec = $sec % 3600;
  $mins = _intdiv($sec, 60);
  $sec = $sec % 60;
  $res = '';
  $comma = false;
  if ($wks != 0) {
  $res = $res . _str($wks) . ' wk';
  $comma = true;
}
  if ($ds != 0) {
  if ($comma) {
  $res = $res . ', ';
};
  $res = $res . _str($ds) . ' d';
  $comma = true;
}
  if ($hrs != 0) {
  if ($comma) {
  $res = $res . ', ';
};
  $res = $res . _str($hrs) . ' hr';
  $comma = true;
}
  if ($mins != 0) {
  if ($comma) {
  $res = $res . ', ';
};
  $res = $res . _str($mins) . ' min';
  $comma = true;
}
  if ($sec != 0) {
  if ($comma) {
  $res = $res . ', ';
};
  $res = $res . _str($sec) . ' sec';
}
  return $res;
}
echo rtrim(timeStr(7259)), PHP_EOL;
echo rtrim(timeStr(86400)), PHP_EOL;
echo rtrim(timeStr(6000000)), PHP_EOL;
