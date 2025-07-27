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
$__start_mem = memory_get_usage();
$__start = _now();
  function floorMod($a, $b) {
  $r = $a % $b;
  if ($r < 0) {
  $r = $r + $b;
}
  return $r;
};
  function run(&$bc) {
  $acc = 0;
  $pc = 0;
  while ($pc < 32) {
  $op = $bc[$pc] / 32;
  $arg = fmod($bc[$pc], 32);
  $pc = $pc + 1;
  if ($op == 0) {
} else {
  if ($op == 1) {
  $acc = $bc[$arg];
} else {
  if ($op == 2) {
  $bc[$arg] = $acc;
} else {
  if ($op == 3) {
  $acc = floorMod($acc + $bc[$arg], 256);
} else {
  if ($op == 4) {
  $acc = floorMod($acc - $bc[$arg], 256);
} else {
  if ($op == 5) {
  if ($acc == 0) {
  $pc = $arg;
};
} else {
  if ($op == 6) {
  $pc = $arg;
} else {
  if ($op == 7) {
  break;
} else {
  break;
};
};
};
};
};
};
};
}
};
  return $acc;
};
  function main() {
  $programs = [[35, 100, 224, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [44, 106, 76, 43, 141, 75, 168, 192, 44, 224, 8, 7, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [46, 79, 109, 78, 47, 77, 48, 145, 171, 80, 192, 46, 224, 1, 1, 0, 8, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [45, 111, 69, 112, 71, 0, 78, 0, 171, 79, 192, 46, 224, 32, 0, 28, 1, 0, 0, 0, 6, 0, 2, 26, 5, 20, 3, 30, 1, 22, 4, 24], [35, 132, 224, 0, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [35, 132, 224, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [35, 100, 224, 1, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]];
  $i = 0;
  while ($i < count($programs)) {
  $res = run($programs[$i]);
  echo rtrim(_str($res)), PHP_EOL;
  $i = $i + 1;
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
