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
  function trimSpace($s) {
  $start = 0;
  while ($start < strlen($s) && substr($s, $start, $start + 1 - $start) == ' ') {
  $start = $start + 1;
};
  $end = strlen($s);
  while ($end > $start && substr($s, $end - 1, $end - ($end - 1)) == ' ') {
  $end = $end - 1;
};
  return substr($s, $start, $end - $start);
};
  function isUpper($ch) {
  return $ch >= 'A' && $ch <= 'Z';
};
  function padLeft($s, $w) {
  $res = '';
  $n = $w - strlen($s);
  while ($n > 0) {
  $res = $res . ' ';
  $n = $n - 1;
};
  return $res . $s;
};
  function snakeToCamel($s) {
  $s = trimSpace($s);
  $out = '';
  $up = false;
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == '_' || $ch == '-' || $ch == ' ' || $ch == '.') {
  $up = true;
  $i = $i + 1;
  continue;
}
  if ($i == 0) {
  $out = $out . strtolower($ch);
  $up = false;
  $i = $i + 1;
  continue;
}
  if ($up) {
  $out = $out . strtoupper($ch);
  $up = false;
} else {
  $out = $out . $ch;
}
  $i = $i + 1;
};
  return $out;
};
  function camelToSnake($s) {
  $s = trimSpace($s);
  $out = '';
  $prevUnd = false;
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == ' ' || $ch == '-' || $ch == '.') {
  if (!$prevUnd && strlen($out) > 0) {
  $out = $out . '_';
  $prevUnd = true;
};
  $i = $i + 1;
  continue;
}
  if ($ch == '_') {
  if (!$prevUnd && strlen($out) > 0) {
  $out = $out . '_';
  $prevUnd = true;
};
  $i = $i + 1;
  continue;
}
  if (isUpper($ch)) {
  if ($i > 0 && (!$prevUnd)) {
  $out = $out . '_';
};
  $out = $out . strtolower($ch);
  $prevUnd = false;
} else {
  $out = $out . strtolower($ch);
  $prevUnd = false;
}
  $i = $i + 1;
};
  $start = 0;
  while ($start < strlen($out) && substr($out, $start, $start + 1 - $start) == '_') {
  $start = $start + 1;
};
  $end = strlen($out);
  while ($end > $start && substr($out, $end - 1, $end - ($end - 1)) == '_') {
  $end = $end - 1;
};
  $out = substr($out, $start, $end - $start);
  $res = '';
  $j = 0;
  $lastUnd = false;
  while ($j < strlen($out)) {
  $c = substr($out, $j, $j + 1 - $j);
  if ($c == '_') {
  if (!$lastUnd) {
  $res = $res . $c;
};
  $lastUnd = true;
} else {
  $res = $res . $c;
  $lastUnd = false;
}
  $j = $j + 1;
};
  return $res;
};
  function main() {
  $samples = ['snakeCase', 'snake_case', 'snake-case', 'snake case', 'snake CASE', 'snake.case', 'variable_10_case', 'variable10Case', 'ɛrgo rE tHis', 'hurry-up-joe!', 'c://my-docs/happy_Flag-Day/12.doc', ' spaces '];
  echo rtrim('=== To snake_case ==='), PHP_EOL;
  foreach ($samples as $s) {
  echo rtrim(padLeft($s, 34) . ' => ' . camelToSnake($s)), PHP_EOL;
};
  echo rtrim(''), PHP_EOL;
  echo rtrim('=== To camelCase ==='), PHP_EOL;
  foreach ($samples as $s) {
  echo rtrim(padLeft($s, 34) . ' => ' . snakeToCamel($s)), PHP_EOL;
};
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
