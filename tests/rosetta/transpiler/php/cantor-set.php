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
  $width = 81;
  $height = 5;
  $lines = [];
  for ($i = 0; $i < $height; $i++) {
  $row = '';
  $j = 0;
  while ($j < $width) {
  $row = $row . '*';
  $j = $j + 1;
};
  $lines = array_merge($lines, [$row]);
}
  function setChar($s, $idx, $ch) {
  global $width, $height, $lines, $row, $j, $stack, $frame, $start, $lenSeg, $index, $seg, $i;
  return substr($s, 0, $idx - 0) . $ch . substr($s, $idx + 1, strlen($s) - ($idx + 1));
};
  $stack = [['start' => 0, 'len' => $width, 'index' => 1]];
  while (count($stack) > 0) {
  $frame = $stack[count($stack) - 1];
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
  $start = $frame['start'];
  $lenSeg = $frame['len'];
  $index = $frame['index'];
  $seg = intval((_intdiv($lenSeg, 3)));
  if ($seg == 0) {
  continue;
}
  $i = $index;
  while ($i < $height) {
  $j = $start + $seg;
  while ($j < $start + 2 * $seg) {
  $lines[$i] = setChar($lines[$i], $j, ' ');
  $j = $j + 1;
};
  $i = $i + 1;
};
  $stack = array_merge($stack, [['start' => $start, 'len' => $seg, 'index' => $index + 1]]);
  $stack = array_merge($stack, [['start' => $start + $seg * 2, 'len' => $seg, 'index' => $index + 1]]);
}
  foreach ($lines as $line) {
  echo rtrim($line), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
