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
  function evaluate($item, $target) {
  $score = 0;
  $i = 0;
  while ($i < strlen($item) && $i < strlen($target)) {
  if (substr($item, $i, $i + 1 - $i) == substr($target, $i, $i + 1 - $i)) {
  $score = $score + 1;
}
  $i = $i + 1;
};
  return $score;
};
  function crossover($parent1, $parent2) {
  $cut = strlen($parent1) / 2;
  $child1 = substr($parent1, 0, $cut) . substr($parent2, $cut, strlen($parent2) - $cut);
  $child2 = substr($parent2, 0, $cut) . substr($parent1, $cut, strlen($parent1) - $cut);
  return ['first' => $child1, 'second' => $child2];
};
  function mutate($child, $genes) {
  if (strlen($child) == 0) {
  return $child;
}
  $gene = $genes[0];
  return substr($child, 0, strlen($child) - 1) . $gene;
};
  function main() {
  echo rtrim(_str(evaluate('Helxo Worlx', 'Hello World'))), PHP_EOL;
  $pair = crossover('123456', 'abcdef');
  echo rtrim(json_encode($pair['first'], 1344)), PHP_EOL;
  echo rtrim(json_encode($pair['second'], 1344)), PHP_EOL;
  $mut = mutate('123456', ['A', 'B', 'C', 'D', 'E', 'F']);
  echo rtrim($mut), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
