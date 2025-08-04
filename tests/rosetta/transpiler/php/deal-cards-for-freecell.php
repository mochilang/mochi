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
  $seed = 1;
  function rnd() {
  global $seed, $suits, $nums;
  $seed = ($seed * 214013 + 2531011) % 2147483648;
  return _intdiv($seed, 65536);
};
  function deal($game) {
  global $seed, $suits, $nums;
  $seed = $game;
  $deck = [];
  $i = 0;
  while ($i < 52) {
  $deck = _append($deck, 51 - $i);
  $i = $i + 1;
};
  $i = 0;
  while ($i < 51) {
  $j = 51 - (fmod(rnd(), (52 - $i)));
  $tmp = $deck[$i];
  $deck[$i] = $deck[$j];
  $deck[$j] = $tmp;
  $i = $i + 1;
};
  return $deck;
};
  $suits = 'CDHS';
  $nums = 'A23456789TJQK';
  function show($cards) {
  global $seed, $suits, $nums;
  $i = 0;
  while ($i < count($cards)) {
  $c = $cards[$i];
  fwrite(STDOUT, ' ' . substr($nums, _intdiv($c, 4), _intdiv($c, 4) + 1 - _intdiv($c, 4)) . substr($suits, $c % 4, $c % 4 + 1 - $c % 4));
  if (($i + 1) % 8 == 0 || $i + 1 == count($cards)) {
  echo rtrim(''), PHP_EOL;
}
  $i = $i + 1;
};
};
  echo rtrim(''), PHP_EOL;
  echo rtrim('Game #1'), PHP_EOL;
  show(deal(1));
  echo rtrim(''), PHP_EOL;
  echo rtrim('Game #617'), PHP_EOL;
  show(deal(617));
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
