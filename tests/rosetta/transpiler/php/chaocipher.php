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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function indexOf($s, $ch) {
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function rotate($s, $n) {
  return substr($s, $n) . substr($s, 0, $n - 0);
};
  function scrambleLeft($s) {
  return substr($s, 0, 1 - 0) . substr($s, 2, 14 - 2) . substr($s, 1, 2 - 1) . substr($s, 14);
};
  function scrambleRight($s) {
  return substr($s, 1, 3 - 1) . substr($s, 4, 15 - 4) . substr($s, 3, 4 - 3) . substr($s, 15) . substr($s, 0, 1 - 0);
};
  function chao($text, $encode) {
  $left = 'HXUCZVAMDSLKPEFJRIGTWOBNYQ';
  $right = 'PTLNBQDEOYSFAVZKGJRIHWXUMC';
  $out = '';
  $i = 0;
  while ($i < strlen($text)) {
  $ch = substr($text, $i, $i + 1 - $i);
  $idx = 0;
  if ($encode) {
  $idx = _indexof($right, $ch);
  $out = $out . substr($left, $idx, $idx + 1 - $idx);
} else {
  $idx = _indexof($left, $ch);
  $out = $out . substr($right, $idx, $idx + 1 - $idx);
}
  $left = rotate($left, $idx);
  $right = rotate($right, $idx);
  $left = scrambleLeft($left);
  $right = scrambleRight($right);
  $i = $i + 1;
};
  return $out;
};
  function main() {
  $plain = 'WELLDONEISBETTERTHANWELLSAID';
  $cipher = chao($plain, true);
  echo rtrim($plain), PHP_EOL;
  echo rtrim($cipher), PHP_EOL;
  echo rtrim(chao($cipher, false)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
