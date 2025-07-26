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
  $seed = 1;
  function randN($n) {
  global $seed;
  $seed = ($seed * 1664525 + 1013904223) % 2147483647;
  return $seed % $n;
};
  function newField($w, $h) {
  global $seed;
  $rows = [];
  $y = 0;
  while ($y < $h) {
  $row = [];
  $x = 0;
  while ($x < $w) {
  $row = array_merge($row, [false]);
  $x = $x + 1;
};
  $rows = array_merge($rows, [$row]);
  $y = $y + 1;
};
  return ['s' => $rows, 'w' => $w, 'h' => $h];
};
  function setCell(&$f, $x, $y, $b) {
  global $seed;
  $rows = $f['s'];
  $row = $rows[$y];
  $row[$x] = $b;
  $rows[$y] = $row;
  $f['s'] = $rows;
};
  function state($f, $x, $y) {
  global $seed;
  while ($y < 0) {
  $y = $y + $f['h'];
};
  while ($x < 0) {
  $x = $x + $f['w'];
};
  return $f['s'][fmod($y, $f['h'])][fmod($x, $f['w'])];
};
  function nextState($f, $x, $y) {
  global $seed;
  $count = 0;
  $dy = -1;
  while ($dy <= 1) {
  $dx = -1;
  while ($dx <= 1) {
  if (!($dx == 0 && $dy == 0) && state($f, $x + $dx, $y + $dy)) {
  $count = $count + 1;
}
  $dx = $dx + 1;
};
  $dy = $dy + 1;
};
  return $count == 3 || ($count == 2 && state($f, $x, $y));
};
  function newLife($w, $h) {
  global $seed;
  $a = newField($w, $h);
  $i = 0;
  while ($i < (_intdiv($w * $h, 2))) {
  setCell($a, randN($w), randN($h), true);
  $i = $i + 1;
};
  return ['a' => $a, 'b' => newField($w, $h), 'w' => $w, 'h' => $h];
};
  function step(&$l) {
  global $seed;
  $y = 0;
  while ($y < $l['h']) {
  $x = 0;
  while ($x < $l['w']) {
  setCell($l['b'], $x, $y, nextState($l['a'], $x, $y));
  $x = $x + 1;
};
  $y = $y + 1;
};
  $tmp = $l['a'];
  $l['a'] = $l['b'];
  $l['b'] = $tmp;
};
  function lifeString($l) {
  global $seed;
  $out = '';
  $y = 0;
  while ($y < $l['h']) {
  $x = 0;
  while ($x < $l['w']) {
  if (state($l['a'], $x, $y)) {
  $out = $out . '*';
} else {
  $out = $out . ' ';
}
  $x = $x + 1;
};
  $out = $out . '\n';
  $y = $y + 1;
};
  return $out;
};
  function main() {
  global $seed;
  $l = newLife(80, 15);
  $i = 0;
  while ($i < 300) {
  step($l);
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
  echo rtrim(lifeString($l)), PHP_EOL;
  $i = $i + 1;
};
}
main();
