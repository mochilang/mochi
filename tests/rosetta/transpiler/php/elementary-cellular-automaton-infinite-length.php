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
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function repeat($s, $n) {
    return str_repeat($s, intval($n));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function pow2($n) {
  $p = 1;
  $i = 0;
  while ($i < $n) {
  $p = $p * 2;
  $i = $i + 1;
};
  return $p;
};
  function btoi($b) {
  if ($b) {
  return 1;
}
  return 0;
};
  function addNoCells($cells) {
  $l = 'O';
  $r = 'O';
  if (substr($cells, 0, 1 - 0) == 'O') {
  $l = '.';
}
  if (substr($cells, strlen($cells) - 1, strlen($cells) - (strlen($cells) - 1)) == 'O') {
  $r = '.';
}
  $cells = $l . $cells . $r;
  $cells = $l . $cells . $r;
  return $cells;
};
  function step($cells, $ruleVal) {
  $newCells = '';
  $i = 0;
  while ($i < strlen($cells) - 2) {
  $bin = 0;
  $b = 2;
  $n = $i;
  while ($n < $i + 3) {
  $bin = $bin + btoi(substr($cells, $n, $n + 1 - $n) == 'O') * pow2($b);
  $b = $b - 1;
  $n = $n + 1;
};
  $a = '.';
  if ((fmod(($ruleVal / pow2($bin)), 2) == 1)) {
  $a = 'O';
}
  $newCells = $newCells . $a;
  $i = $i + 1;
};
  return $newCells;
};
  function mochi_repeat($ch, $n) {
  $s = '';
  $i = 0;
  while ($i < $n) {
  $s = $s . $ch;
  $i = $i + 1;
};
  return $s;
};
  function evolve($l, $ruleVal) {
  echo rtrim(' Rule #' . _str($ruleVal) . ':'), PHP_EOL;
  $cells = 'O';
  $x = 0;
  while ($x < $l) {
  $cells = addNoCells($cells);
  $width = 40 + (strlen($cells) / 2);
  $spaces = repeat(' ', $width - strlen($cells));
  echo rtrim($spaces . $cells), PHP_EOL;
  $cells = step($cells, $ruleVal);
  $x = $x + 1;
};
};
  function main() {
  foreach ([90, 30] as $r) {
  evolve(25, $r);
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
