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
  function bitAt($x, $idx) {
  $v = $x;
  $i = 0;
  while ($i < $idx) {
  $v = intval((_intdiv($v, 2)));
  $i = $i + 1;
};
  return $v % 2;
};
  function outputState($state) {
  $line = '';
  $i = 0;
  while ($i < strlen($state)) {
  if (substr($state, $i, $i + 1 - $i) == '1') {
  $line = $line . '#';
} else {
  $line = $line . ' ';
}
  $i = $i + 1;
};
  echo rtrim($line), PHP_EOL;
};
  function step($state, $r) {
  $cells = strlen($state);
  $out = '';
  $i = 0;
  while ($i < $cells) {
  $l = substr($state, ($i - 1 + $cells) % $cells, ($i - 1 + $cells) % $cells + 1 - ($i - 1 + $cells) % $cells);
  $c = substr($state, $i, $i + 1 - $i);
  $rt = substr($state, ($i + 1) % $cells, ($i + 1) % $cells + 1 - ($i + 1) % $cells);
  $idx = 0;
  if ($l == '1') {
  $idx = $idx + 4;
}
  if ($c == '1') {
  $idx = $idx + 2;
}
  if ($rt == '1') {
  $idx = $idx + 1;
}
  if (bitAt($r, $idx) == 1) {
  $out = $out . '1';
} else {
  $out = $out . '0';
}
  $i = $i + 1;
};
  return $out;
};
  function elem($r, $cells, $generations, $state) {
  outputState($state);
  $g = 0;
  $s = $state;
  while ($g < $generations) {
  $s = step($s, $r);
  outputState($s);
  $g = $g + 1;
};
};
  function randInit($cells, $seed) {
  $s = '';
  $val = $seed;
  $i = 0;
  while ($i < $cells) {
  $val = ($val * 1664525 + 1013904223) % 2147483647;
  if ($val % 2 == 0) {
  $s = $s . '0';
} else {
  $s = $s . '1';
}
  $i = $i + 1;
};
  return $s;
};
  function singleInit($cells) {
  $s = '';
  $i = 0;
  while ($i < $cells) {
  if ($i == _intdiv($cells, 2)) {
  $s = $s . '1';
} else {
  $s = $s . '0';
}
  $i = $i + 1;
};
  return $s;
};
  function main() {
  $cells = 20;
  $generations = 9;
  echo rtrim('Single 1, rule 90:'), PHP_EOL;
  $state = singleInit($cells);
  elem(90, $cells, $generations, $state);
  echo rtrim('Random intial state, rule 30:'), PHP_EOL;
  $state = randInit($cells, 3);
  elem(30, $cells, $generations, $state);
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
