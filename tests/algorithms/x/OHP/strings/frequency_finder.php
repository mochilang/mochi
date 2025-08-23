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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $ETAOIN = 'ETAOINSHRDLCUMWFGYPBVKJXQZ';
  $LETTERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  function etaoin_index($letter) {
  global $ETAOIN, $LETTERS;
  $i = 0;
  while ($i < strlen($ETAOIN)) {
  if (substr($ETAOIN, $i, $i + 1 - $i) == $letter) {
  return $i;
}
  $i = $i + 1;
};
  return strlen($ETAOIN);
};
  function get_letter_count($message) {
  global $ETAOIN, $LETTERS;
  $letter_count = [];
  $i = 0;
  while ($i < strlen($LETTERS)) {
  $c = substr($LETTERS, $i, $i + 1 - $i);
  $letter_count[$c] = 0;
  $i = $i + 1;
};
  $msg = strtoupper($message);
  $j = 0;
  while ($j < strlen($msg)) {
  $ch = substr($msg, $j, $j + 1 - $j);
  if (strpos($LETTERS, $ch) !== false) {
  $letter_count[$ch] = $letter_count[$ch] + 1;
}
  $j = $j + 1;
};
  return $letter_count;
};
  function get_frequency_order($message) {
  global $ETAOIN, $LETTERS;
  $letter_to_freq = get_letter_count($message);
  $max_freq = 0;
  $i = 0;
  while ($i < strlen($LETTERS)) {
  $letter = substr($LETTERS, $i, $i + 1 - $i);
  $f = $letter_to_freq[$letter];
  if ($f > $max_freq) {
  $max_freq = $f;
}
  $i = $i + 1;
};
  $result = '';
  $freq = $max_freq;
  while ($freq >= 0) {
  $group = [];
  $j = 0;
  while ($j < strlen($LETTERS)) {
  $letter = substr($LETTERS, $j, $j + 1 - $j);
  if ($letter_to_freq[$letter] == $freq) {
  $group = _append($group, $letter);
}
  $j = $j + 1;
};
  $g_len = count($group);
  $a = 0;
  while ($a < $g_len) {
  $b = 0;
  while ($b < $g_len - $a - 1) {
  $g1 = $group[$b];
  $g2 = $group[$b + 1];
  $idx1 = etaoin_index($g1);
  $idx2 = etaoin_index($g2);
  if ($idx1 < $idx2) {
  $tmp = $group[$b];
  $group[$b] = $group[$b + 1];
  $group[$b + 1] = $tmp;
}
  $b = $b + 1;
};
  $a = $a + 1;
};
  $g = 0;
  while ($g < count($group)) {
  $result = $result . $group[$g];
  $g = $g + 1;
};
  $freq = $freq - 1;
};
  return $result;
};
  function english_freq_match_score($message) {
  global $ETAOIN, $LETTERS;
  $freq_order = get_frequency_order($message);
  $top = substr($freq_order, 0, 6);
  $bottom = substr($freq_order, strlen($freq_order) - 6, strlen($freq_order) - (strlen($freq_order) - 6));
  $score = 0;
  $i = 0;
  while ($i < 6) {
  $c = substr($ETAOIN, $i, $i + 1 - $i);
  if (strpos($top, $c) !== false) {
  $score = $score + 1;
}
  $i = $i + 1;
};
  $j = strlen($ETAOIN) - 6;
  while ($j < strlen($ETAOIN)) {
  $c = substr($ETAOIN, $j, $j + 1 - $j);
  if (strpos($bottom, $c) !== false) {
  $score = $score + 1;
}
  $j = $j + 1;
};
  return $score;
};
  function main() {
  global $ETAOIN, $LETTERS;
  echo rtrim(get_frequency_order('Hello World')), PHP_EOL;
  echo rtrim(json_encode(english_freq_match_score('Hello World'), 1344)), PHP_EOL;
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
