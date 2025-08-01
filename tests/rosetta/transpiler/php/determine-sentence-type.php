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
  function mochi_join($xs, $sep) {
  $res = '';
  $i = 0;
  while ($i < count($xs)) {
  if ($i > 0) {
  $res = $res . $sep;
}
  $res = $res . $xs[$i];
  $i = $i + 1;
};
  return $res;
};
  function sentenceType($s) {
  if (strlen($s) == 0) {
  return '';
}
  $types = [];
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == '?') {
  $types = array_merge($types, ['Q']);
} else {
  if ($ch == '!') {
  $types = array_merge($types, ['E']);
} else {
  if ($ch == '.') {
  $types = array_merge($types, ['S']);
};
};
}
  $i = $i + 1;
};
  $last = substr($s, strlen($s) - 1, strlen($s) - (strlen($s) - 1));
  if (_indexof('?!.', $last) == (-1)) {
  $types = array_merge($types, ['N']);
}
  return mochi_join($types, '|');
};
  function main() {
  $s = 'hi there, how are you today? I\'d like to present to you the washing machine 9001. You have been nominated to win one of these! Just make sure you don\'t break it';
  $result = sentenceType($s);
  echo rtrim($result), PHP_EOL;
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
