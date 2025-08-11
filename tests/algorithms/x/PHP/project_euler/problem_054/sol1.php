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
  function card_value($c) {
  if ($c == '2') {
  return 2;
}
  if ($c == '3') {
  return 3;
}
  if ($c == '4') {
  return 4;
}
  if ($c == '5') {
  return 5;
}
  if ($c == '6') {
  return 6;
}
  if ($c == '7') {
  return 7;
}
  if ($c == '8') {
  return 8;
}
  if ($c == '9') {
  return 9;
}
  if ($c == 'T') {
  return 10;
}
  if ($c == 'J') {
  return 11;
}
  if ($c == 'Q') {
  return 12;
}
  if ($c == 'K') {
  return 13;
}
  if ($c == 'A') {
  return 14;
}
  return 0;
};
  function sort_desc($xs) {
  $arr = $xs;
  $n = count($arr);
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < _isub($n, 1)) {
  if ($arr[$j] < $arr[_iadd($j, 1)]) {
  $tmp = $arr[$j];
  $arr[$j] = $arr[_iadd($j, 1)];
  $arr[_iadd($j, 1)] = $tmp;
}
  $j = _iadd($j, 1);
};
  $i = _iadd($i, 1);
};
  return $arr;
};
  function filter_not($xs, $v) {
  $res = [];
  foreach ($xs as $x) {
  if ($x != $v) {
  $res = _append($res, $x);
}
};
  return $res;
};
  function hand_rank($hand) {
  $ranks = [];
  $suits = [];
  foreach ($hand as $card) {
  $ranks = _append($ranks, card_value($card[0]));
  $suits = _append($suits, $card[1]);
};
  $ranks = sort_desc($ranks);
  $flush = true;
  $i = 1;
  while ($i < count($suits)) {
  if ($suits[$i] != $suits[0]) {
  $flush = false;
}
  $i = _iadd($i, 1);
};
  $straight = true;
  $i = 0;
  while ($i < 4) {
  if (_isub($ranks[$i], $ranks[_iadd($i, 1)]) != 1) {
  $straight = false;
}
  $i = _iadd($i, 1);
};
  if (!$straight && $ranks[0] == 14 && $ranks[1] == 5 && $ranks[2] == 4 && $ranks[3] == 3 && $ranks[4] == 2) {
  $straight = true;
  $ranks = [5, 4, 3, 2, 1];
}
  $counts = [];
  foreach ($ranks as $r) {
  if (array_key_exists($r, $counts)) {
  $counts[$r] = _iadd($counts[$r], 1);
} else {
  $counts[$r] = 1;
}
};
  $uniq = [];
  foreach ($ranks as $r) {
  $exists = false;
  foreach ($uniq as $u) {
  if ($u == $r) {
  $exists = true;
}
};
  if (!$exists) {
  $uniq = _append($uniq, $r);
}
};
  $count_vals = [];
  foreach ($uniq as $u) {
  $count_vals = _append($count_vals, [$counts[$u], $u]);
};
  $n = count($count_vals);
  $i2 = 0;
  while ($i2 < $n) {
  $j2 = 0;
  while ($j2 < _isub($n, 1)) {
  $a = $count_vals[$j2];
  $b = $count_vals[_iadd($j2, 1)];
  if ($a[0] < $b[0] || ($a[0] == $b[0] && $a[1] < $b[1])) {
  $tmp = $count_vals[$j2];
  $count_vals[$j2] = $count_vals[_iadd($j2, 1)];
  $count_vals[_iadd($j2, 1)] = $tmp;
}
  $j2 = _iadd($j2, 1);
};
  $i2 = _iadd($i2, 1);
};
  $c1 = $count_vals[0][0];
  $v1 = $count_vals[0][1];
  $rank = [];
  if ($straight && $flush) {
  if ($ranks[0] == 14) {
  $rank = _append($rank, 9);
  $rank = _append($rank, 14);
  return $rank;
};
  $rank = _append($rank, 8);
  $rank = _append($rank, $ranks[0]);
  return $rank;
}
  if ($c1 == 4) {
  $kicker = 0;
  foreach ($ranks as $r) {
  if ($r != $v1) {
  $kicker = $r;
}
};
  $rank = _append($rank, 7);
  $rank = _append($rank, $v1);
  $rank = _append($rank, $kicker);
  return $rank;
}
  if ($c1 == 3) {
  $c2 = $count_vals[1][0];
  $v2 = $count_vals[1][1];
  if ($c2 == 2) {
  $rank = _append($rank, 6);
  $rank = _append($rank, $v1);
  $rank = _append($rank, $v2);
  return $rank;
};
  $rank = _append($rank, 3);
  $rank = _append($rank, $v1);
  foreach ($ranks as $r) {
  if ($r != $v1) {
  $rank = _append($rank, $r);
}
};
  return $rank;
}
  if ($c1 == 2) {
  $c2 = $count_vals[1][0];
  $v2 = $count_vals[1][1];
  if ($c2 == 2) {
  $high_pair = $v1;
  $low_pair = $v2;
  if ($low_pair > $high_pair) {
  $tmp = $high_pair;
  $high_pair = $low_pair;
  $low_pair = $tmp;
};
  $kicker = 0;
  foreach ($ranks as $r) {
  if ($r != $high_pair && $r != $low_pair) {
  $kicker = $r;
}
};
  $rank = _append($rank, 2);
  $rank = _append($rank, $high_pair);
  $rank = _append($rank, $low_pair);
  $rank = _append($rank, $kicker);
  return $rank;
};
  $rank = _append($rank, 1);
  $rank = _append($rank, $v1);
  foreach ($ranks as $r) {
  if ($r != $v1) {
  $rank = _append($rank, $r);
}
};
  return $rank;
}
  if ($flush) {
  $rank = _append($rank, 5);
  foreach ($ranks as $r) {
  $rank = _append($rank, $r);
};
  return $rank;
}
  if ($straight) {
  $rank = _append($rank, 4);
  $rank = _append($rank, $ranks[0]);
  return $rank;
}
  $rank = _append($rank, 0);
  foreach ($ranks as $r) {
  $rank = _append($rank, $r);
};
  return $rank;
};
  function compare_hands($h1, $h2) {
  $r1 = hand_rank($h1);
  $r2 = hand_rank($h2);
  $i = 0;
  while ($i < count($r1) && $i < count($r2)) {
  if ($r1[$i] > $r2[$i]) {
  return 1;
}
  if ($r1[$i] < $r2[$i]) {
  return -1;
}
  $i = _iadd($i, 1);
};
  return 0;
};
  function solution() {
  $hands = [];
  $wins = 0;
  foreach ($hands as $h) {
  $p1 = [$h['c0'], $h['c1'], $h['c2'], $h['c3'], $h['c4']];
  $p2 = [$h['c5'], $h['c6'], $h['c7'], $h['c8'], $h['c9']];
  if (compare_hands($p1, $p2) == 1) {
  $wins = _iadd($wins, 1);
}
};
  return $wins;
};
  echo rtrim(json_encode(solution(), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
