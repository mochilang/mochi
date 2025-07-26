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
  $stx = '';
  $etx = '';
  function contains($s, $ch) {
  global $stx, $etx;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function sortStrings($xs) {
  global $stx, $etx;
  $arr = $xs;
  $n = count($arr);
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < $n - 1) {
  if ($arr[$j] > $arr[$j + 1]) {
  $tmp = $arr[$j];
  $arr[$j] = $arr[$j + 1];
  $arr[$j + 1] = $tmp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
};
  function bwt($s) {
  global $stx, $etx;
  if (str_contains($s, $stx) || str_contains($s, $etx)) {
  return ['err' => true, 'res' => ''];
}
  $s = $stx . $s . $etx;
  $le = strlen($s);
  $table = [];
  $i = 0;
  while ($i < $le) {
  $rot = substr($s, $i, $le - $i) . substr($s, 0, $i - 0);
  $table = array_merge($table, [$rot]);
  $i = $i + 1;
};
  $table = sortStrings($table);
  $last = '';
  $i = 0;
  while ($i < $le) {
  $last = $last . substr($table[$i], $le - 1, $le - ($le - 1));
  $i = $i + 1;
};
  return ['err' => false, 'res' => $last];
};
  function ibwt($r) {
  global $stx, $etx;
  $le = strlen($r);
  $table = [];
  $i = 0;
  while ($i < $le) {
  $table = array_merge($table, ['']);
  $i = $i + 1;
};
  $n = 0;
  while ($n < $le) {
  $i = 0;
  while ($i < $le) {
  $table[$i] = substr($r, $i, $i + 1 - $i) . $table[$i];
  $i = $i + 1;
};
  $table = sortStrings($table);
  $n = $n + 1;
};
  $i = 0;
  while ($i < $le) {
  if (substr($table[$i], $le - 1, $le - ($le - 1)) == $etx) {
  return substr($table[$i], 1, $le - 1 - 1);
}
  $i = $i + 1;
};
  return '';
};
  function makePrintable($s) {
  global $stx, $etx;
  $out = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == $stx) {
  $out = $out . '^';
} else {
  if ($ch == $etx) {
  $out = $out . '|';
} else {
  $out = $out . $ch;
};
}
  $i = $i + 1;
};
  return $out;
};
  function main() {
  global $stx, $etx;
  $examples = ['banana', 'appellee', 'dogwood', 'TO BE OR NOT TO BE OR WANT TO BE OR NOT?', 'SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES', 'ABC'];
  foreach ($examples as $t) {
  echo rtrim(makePrintable($t)), PHP_EOL;
  $res = bwt($t);
  if ($res['err']) {
  echo rtrim(' --> ERROR: String can\'t contain STX or ETX'), PHP_EOL;
  echo rtrim(' -->'), PHP_EOL;
} else {
  $enc = strval($res['res']);
  echo rtrim(' --> ' . makePrintable($enc)), PHP_EOL;
  $r = ibwt($enc);
  echo rtrim(' --> ' . $r), PHP_EOL;
}
  echo rtrim(''), PHP_EOL;
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
