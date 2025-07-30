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
function repeat($s, $n) {
    return str_repeat($s, intval($n));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_repeat($s, $n) {
  global $lines, $blocks, $outLines;
  $out = '';
  $i = 0;
  while ($i < $n) {
  $out = $out . $s;
  $i = $i + 1;
};
  return $out;
};
  function trimRightSpace($s) {
  global $lines, $blocks, $outLines;
  $i = strlen($s) - 1;
  while ($i >= 0 && substr($s, $i, $i + 1 - $i) == ' ') {
  $i = $i - 1;
};
  return substr($s, 0, $i + 1 - 0);
};
  function block2text($block) {
  global $lines, $blocks, $outLines;
  $out = [];
  foreach ($block as $b) {
  $out = array_merge($out, [trimRightSpace($b)]);
};
  return $out;
};
  function text2block($lines) {
  global $blocks, $outLines;
  $out = [];
  $count = 0;
  foreach ($lines as $line) {
  $s = $line;
  $le = strlen($s);
  if ($le > 64) {
  $s = substr($s, 0, 64 - 0);
} else {
  if ($le < 64) {
  $s = $s . repeat(' ', 64 - $le);
};
}
  $out = array_merge($out, [$s]);
  $count = $count + 1;
};
  if ($count % 16 != 0) {
  $pad = 16 - $count % 16;
  $i = 0;
  while ($i < $pad) {
  $out = array_merge($out, [repeat(' ', 64)]);
  $i = $i + 1;
};
}
  return $out;
};
  $lines = ['alpha', 'beta', 'gamma'];
  $blocks = text2block($lines);
  $outLines = block2text($blocks);
  foreach ($outLines as $l) {
  if ($l != '') {
  echo rtrim($l), PHP_EOL;
}
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
