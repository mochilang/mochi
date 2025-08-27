<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function _repeat($s, $n) {
    return str_repeat($s, intval($n));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        $q = bcdiv($sa, $sb, 0);
        $rem = bcmod($sa, $sb);
        $neg = ((strpos($sa, '-') === 0) xor (strpos($sb, '-') === 0));
        if ($neg && bccomp($rem, '0') != 0) {
            $q = bcsub($q, '1');
        }
        return intval($q);
    }
    $ai = intval($a);
    $bi = intval($b);
    $q = intdiv($ai, $bi);
    if ((($ai ^ $bi) < 0) && ($ai % $bi != 0)) {
        $q -= 1;
    }
    return $q;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $digitMap = ['0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9];
  function mochi_repeat($s, $n) {
  global $digitMap;
  $r = '';
  for ($_ = 0; $_ < $n; $_++) {
  $r = $r . $s;
};
  return $r;
};
  function add_str($a, $b) {
  global $digitMap;
  $i = strlen($a) - 1;
  $j = strlen($b) - 1;
  $carry = 0;
  $res = '';
  while ($i >= 0 || $j >= 0 || $carry > 0) {
  $da = 0;
  if ($i >= 0) {
  $da = intval($digitMap[$a[$i]]);
}
  $db = 0;
  if ($j >= 0) {
  $db = intval($digitMap[$b[$j]]);
}
  $sum = $da + $db + $carry;
  $res = _str($sum % 10) . $res;
  $carry = _intdiv($sum, 10);
  $i = $i - 1;
  $j = $j - 1;
};
  return $res;
};
  function sub_str($a, $b) {
  global $digitMap;
  $i = strlen($a) - 1;
  $j = strlen($b) - 1;
  $borrow = 0;
  $res = '';
  while ($i >= 0) {
  $da = intval($digitMap[$a[$i]]) - $borrow;
  $db = 0;
  if ($j >= 0) {
  $db = intval($digitMap[$b[$j]]);
}
  if ($da < $db) {
  $da = $da + 10;
  $borrow = 1;
} else {
  $borrow = 0;
}
  $diff = $da - $db;
  $res = _str($diff) . $res;
  $i = $i - 1;
  $j = $j - 1;
};
  $k = 0;
  while ($k < strlen($res) && substr($res, $k, $k + 1 - $k) == '0') {
  $k = $k + 1;
};
  if ($k == strlen($res)) {
  return '0';
}
  return substr($res, $k);
};
  function mul_digit($a, $d) {
  global $digitMap;
  if ($d == 0) {
  return '0';
}
  $i = strlen($a) - 1;
  $carry = 0;
  $res = '';
  while ($i >= 0) {
  $prod = intval($digitMap[$a[$i]]) * $d + $carry;
  $res = _str($prod % 10) . $res;
  $carry = _intdiv($prod, 10);
  $i = $i - 1;
};
  if ($carry > 0) {
  $res = _str($carry) . $res;
}
  $k = 0;
  while ($k < strlen($res) && substr($res, $k, $k + 1 - $k) == '0') {
  $k = $k + 1;
};
  if ($k == strlen($res)) {
  return '0';
}
  return substr($res, $k);
};
  function mul_str($a, $b) {
  global $digitMap;
  $result = '0';
  $shift = 0;
  $parts = [];
  $i = strlen($b) - 1;
  while ($i >= 0) {
  $d = intval($digitMap[$b[$i]]);
  $part = mul_digit($a, $d);
  $parts = _append($parts, ['val' => &$part, 'shift' => &$shift]);
  $shifted = $part;
  for ($_ = 0; $_ < $shift; $_++) {
  $shifted = $shifted . '0';
};
  $result = add_str($result, $shifted);
  $shift = $shift + 1;
  $i = $i - 1;
};
  return ['parts' => &$parts, 'res' => &$result];
};
  function pad_left($s, $total) {
  global $digitMap;
  $r = '';
  for ($_ = 0; $_ < ($total - strlen($s)); $_++) {
  $r = $r . ' ';
};
  return $r . $s;
};
  function main() {
  global $digitMap;
  $tStr = trim(fgets(STDIN));
  if ($tStr == '') {
  return;
}
  $t = intval($tStr);
  for ($_ = 0; $_ < $t; $_++) {
  $line = trim(fgets(STDIN));
  if ($line == '') {
  continue;
}
  $idx = 0;
  while ($idx < strlen($line)) {
  $ch = substr($line, $idx, $idx + 1 - $idx);
  if ($ch == '+' || $ch == '-' || $ch == '*') {
  break;
}
  $idx = $idx + 1;
};
  $a = substr($line, 0, $idx);
  $op = substr($line, $idx, $idx + 1 - $idx);
  $b = substr($line, $idx + 1);
  $res = '';
  $parts = [];
  if ($op == '+') {
  $res = add_str($a, $b);
} else {
  if ($op == '-') {
  $res = sub_str($a, $b);
} else {
  $r = mul_str($a, $b);
  $res = strval($r['res']);
  $parts = $r['parts'];
};
}
  $width = strlen($a);
  $secondLen = strlen($b) + 1;
  if ($secondLen > $width) {
  $width = $secondLen;
}
  if (strlen($res) > $width) {
  $width = strlen($res);
}
  foreach ($parts as $p) {
  $l = strlen(strval($p['val'])) + (intval($p['shift']));
  if ($l > $width) {
  $width = $l;
}
};
  echo rtrim(pad_left($a, $width)), PHP_EOL;
  echo rtrim(pad_left($op . $b, $width)), PHP_EOL;
  $dash1 = 0;
  if ($op == '*') {
  if (count($parts) > 0) {
  $dash1 = strlen($b) + 1;
  $firstPart = strval($parts[0]['val']);
  if (strlen($firstPart) > $dash1) {
  $dash1 = strlen($firstPart);
};
} else {
  $dash1 = strlen($b) + 1;
  if (strlen($res) > $dash1) {
  $dash1 = strlen($res);
};
};
} else {
  $dash1 = strlen($b) + 1;
  if (strlen($res) > $dash1) {
  $dash1 = strlen($res);
};
}
  echo rtrim(pad_left(_repeat('-', $dash1), $width)), PHP_EOL;
  if ($op == '*' && strlen($b) > 1) {
  foreach ($parts as $p) {
  $val = strval($p['val']);
  $shift = intval($p['shift']);
  $spaces = $width - $shift - strlen($val);
  $line = '';
  for ($_ = 0; $_ < $spaces; $_++) {
  $line = $line . ' ';
};
  $line = $line . $val;
  echo rtrim($line), PHP_EOL;
};
  echo rtrim(pad_left(_repeat('-', strlen($res)), $width)), PHP_EOL;
}
  echo rtrim(pad_left($res, $width)), PHP_EOL;
  echo rtrim(''), PHP_EOL;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
