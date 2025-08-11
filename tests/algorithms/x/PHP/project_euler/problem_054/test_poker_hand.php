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
function _len($x) {
    if ($x === null) { return 0; }
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function split($s, $sep) {
  $parts = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  if (strlen($sep) > 0 && _iadd($i, strlen($sep)) <= strlen($s) && substr($s, $i, _iadd($i, strlen($sep)) - $i) == $sep) {
  $parts = _append($parts, $cur);
  $cur = '';
  $i = _iadd($i, strlen($sep));
} else {
  $cur = $cur . substr($s, $i, _iadd($i, 1) - $i);
  $i = _iadd($i, 1);
}
};
  $parts = _append($parts, $cur);
  return $parts;
};
  function card_value($ch) {
  if ($ch == 'A') {
  return 14;
} else {
  if ($ch == 'K') {
  return 13;
} else {
  if ($ch == 'Q') {
  return 12;
} else {
  if ($ch == 'J') {
  return 11;
} else {
  if ($ch == 'T') {
  return 10;
} else {
  if ($ch == '9') {
  return 9;
} else {
  if ($ch == '8') {
  return 8;
} else {
  if ($ch == '7') {
  return 7;
} else {
  if ($ch == '6') {
  return 6;
} else {
  if ($ch == '5') {
  return 5;
} else {
  if ($ch == '4') {
  return 4;
} else {
  if ($ch == '3') {
  return 3;
} else {
  return 2;
};
};
};
};
};
};
};
};
};
};
};
}
};
  function parse_hand($hand) {
  $counts = [];
  $i = 0;
  while ($i <= 14) {
  $counts = _append($counts, 0);
  $i = _iadd($i, 1);
};
  $suits = [];
  foreach (explode(' ', $hand) as $card) {
  $v = card_value(substr($card, 0, 1));
  $counts[$v] = _iadd($counts[$v], 1);
  $suits = _append($suits, $card[1]);
};
  $vals = [];
  $v = 14;
  while ($v >= 2) {
  $c = $counts[$v];
  $k = 0;
  while ($k < $c) {
  $vals = _append($vals, $v);
  $k = _iadd($k, 1);
};
  $v = _isub($v, 1);
};
  $is_straight = false;
  if (count($vals) == 5 && $vals[0] == 14 && $vals[1] == 5 && $vals[2] == 4 && $vals[3] == 3 && $vals[4] == 2) {
  $is_straight = true;
  $vals[0] = 5;
  $vals[1] = 4;
  $vals[2] = 3;
  $vals[3] = 2;
  $vals[4] = 14;
} else {
  $is_straight = true;
  $j = 0;
  while ($j < 4) {
  if (_isub($vals[$j], $vals[_iadd($j, 1)]) != 1) {
  $is_straight = false;
}
  $j = _iadd($j, 1);
};
}
  $is_flush = true;
  $s0 = $suits[0];
  $t = 1;
  while ($t < count($suits)) {
  if ($suits[$t] != $s0) {
  $is_flush = false;
}
  $t = _iadd($t, 1);
};
  $four_val = 0;
  $three_val = 0;
  $pair_vals = [];
  $v = 14;
  while ($v >= 2) {
  if ($counts[$v] == 4) {
  $four_val = $v;
} else {
  if ($counts[$v] == 3) {
  $three_val = $v;
} else {
  if ($counts[$v] == 2) {
  $pair_vals = _append($pair_vals, $v);
};
};
}
  $v = _isub($v, 1);
};
  $rank = 1;
  if ($is_flush && $is_straight && $vals[0] == 14 && $vals[4] == 10) {
  $rank = 10;
} else {
  if ($is_flush && $is_straight) {
  $rank = 9;
} else {
  if ($four_val != 0) {
  $rank = 8;
} else {
  if ($three_val != 0 && count($pair_vals) == 1) {
  $rank = 7;
} else {
  if ($is_flush) {
  $rank = 6;
} else {
  if ($is_straight) {
  $rank = 5;
} else {
  if ($three_val != 0) {
  $rank = 4;
} else {
  if (count($pair_vals) == 2) {
  $rank = 3;
} else {
  if (count($pair_vals) == 1) {
  $rank = 2;
} else {
  $rank = 1;
};
};
};
};
};
};
};
};
}
  return ['rank' => $rank, 'values' => $vals];
};
  function compare($a, $b) {
  if ($a['rank'] > $b['rank']) {
  return 'Win';
}
  if ($a['rank'] < $b['rank']) {
  return 'Loss';
}
  $i = 0;
  while ($i < _len($a['values'])) {
  if ($a['values'][$i] > $b['values'][$i]) {
  return 'Win';
}
  if ($a['values'][$i] < $b['values'][$i]) {
  return 'Loss';
}
  $i = _iadd($i, 1);
};
  return 'Tie';
};
  function main() {
  $tests = [['2H 3H 4H 5H 6H', 'KS AS TS QS JS', 'Loss'], ['2H 3H 4H 5H 6H', 'AS AD AC AH JD', 'Win'], ['AS AH 2H AD AC', 'JS JD JC JH 3D', 'Win'], ['2S AH 2H AS AC', 'JS JD JC JH AD', 'Loss'], ['2S AH 2H AS AC', '2H 3H 5H 6H 7H', 'Win']];
  foreach ($tests as $t) {
  $res = compare(parse_hand($t[0]), parse_hand($t[1]));
  echo rtrim($res . ' expected ' . $t[2]), PHP_EOL;
};
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
