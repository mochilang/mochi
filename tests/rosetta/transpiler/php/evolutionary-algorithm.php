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
$__start_mem = memory_get_usage();
$__start = _now();
  $target = 'METHINKS IT IS LIKE A WEASEL';
  $chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ ';
  $seed = 1;
  function randInt($s, $n) {
  global $target, $chars, $seed;
  $next = ($s * 1664525 + 1013904223) % 2147483647;
  return [$next, $next % $n];
};
  function randChar() {
  global $target, $chars, $seed;
  $r = randInt($seed, strlen($chars));
  $seed = $r[0];
  $idx = intval($r[1]);
  return substr($chars, $idx, $idx + 1 - $idx);
};
  function randomString($n) {
  global $target, $chars, $seed;
  $s = '';
  $i = 0;
  while ($i < $n) {
  $s = $s . randChar();
  $i = $i + 1;
};
  return $s;
};
  function fitness($s) {
  global $target, $chars, $seed;
  $h = 0;
  $i = 0;
  while ($i < strlen($target)) {
  if (substr($s, $i, $i + 1 - $i) != substr($target, $i, $i + 1 - $i)) {
  $h = $h + 1;
}
  $i = $i + 1;
};
  return $h;
};
  function mutate($p) {
  global $target, $chars, $seed;
  $m = '';
  $i = 0;
  while ($i < strlen($p)) {
  $r = randInt($seed, 20);
  $seed = $r[0];
  if ($r[1] == 0) {
  $m = $m . randChar();
} else {
  $m = $m . substr($p, $i, $i + 1 - $i);
}
  $i = $i + 1;
};
  return $m;
};
  function main() {
  global $target, $chars, $seed;
  $parent = randomString(strlen($target));
  echo rtrim($parent), PHP_EOL;
  $best = fitness($parent);
  $done = false;
  while (!$done) {
  $i = 0;
  while ($i < 20) {
  $child = mutate($parent);
  $f = fitness($child);
  if ($f < $best) {
  $best = $f;
  $parent = $child;
  echo rtrim($parent), PHP_EOL;
  if ($best == 0) {
  $done = true;
  break;
};
}
  $i = $i + 1;
};
};
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
