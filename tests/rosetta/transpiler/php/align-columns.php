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
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function split($s, $sep) {
  global $text, $f;
  $parts = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  if (strlen($sep) > 0 && $i + strlen($sep) <= strlen($s) && substr($s, $i, $i + strlen($sep) - $i) == $sep) {
  $parts = array_merge($parts, [$cur]);
  $cur = '';
  $i = $i + strlen($sep);
} else {
  $cur = $cur . substr($s, $i, $i + 1 - $i);
  $i = $i + 1;
}
};
  $parts = array_merge($parts, [$cur]);
  return $parts;
};
  function rstripEmpty($words) {
  global $text, $f;
  $n = count($words);
  while ($n > 0 && $words[$n - 1] == '') {
  $n = $n - 1;
};
  return array_slice($words, 0, $n - 0);
};
  function spaces($n) {
  global $text, $f;
  $out = '';
  $i = 0;
  while ($i < $n) {
  $out = $out . ' ';
  $i = $i + 1;
};
  return $out;
};
  function pad($word, $width, $align) {
  global $text, $f;
  $diff = $width - strlen($word);
  if ($align == 0) {
  return $word . spaces($diff);
}
  if ($align == 2) {
  return spaces($diff) . $word;
}
  $left = intval((_intdiv($diff, 2)));
  $right = $diff - $left;
  return spaces($left) . $word . spaces($right);
};
  function newFormatter($text) {
  global $f;
  $lines = explode('\n', $text);
  $fmtLines = [];
  $width = [];
  $i = 0;
  while ($i < count($lines)) {
  if (strlen($lines[$i]) == 0) {
  $i = $i + 1;
  continue;
}
  $words = rstripEmpty(explode('$', $lines[$i]));
  $fmtLines = array_merge($fmtLines, [$words]);
  $j = 0;
  while ($j < count($words)) {
  $wlen = strlen($words[$j]);
  if ($j == count($width)) {
  $width = array_merge($width, [$wlen]);
} else {
  if ($wlen > $width[$j]) {
  $width[$j] = $wlen;
};
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return ['text' => $fmtLines, 'width' => $width];
};
  function printFmt($f, $align) {
  global $text;
  $lines = $f['text'];
  $width = $f['width'];
  $i = 0;
  while ($i < count($lines)) {
  $words = $lines[$i];
  $line = '';
  $j = 0;
  while ($j < count($words)) {
  $line = $line . pad($words[$j], $width[$j], $align) . ' ';
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
  echo rtrim(''), PHP_EOL;
};
  $text = 'Given$a$text$file$of$many$lines,$where$fields$within$a$line\n' . 'are$delineated$by$a$single$\'dollar\'$character,$write$a$program\n' . 'that$aligns$each$column$of$fields$by$ensuring$that$words$in$each\n' . 'column$are$separated$by$at$least$one$space.\n' . 'Further,$allow$for$each$word$in$a$column$to$be$either$left\n' . 'justified,$right$justified,$or$center$justified$within$its$column.';
  $f = newFormatter($text);
  printFmt($f, 0);
  printFmt($f, 1);
  printFmt($f, 2);
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
