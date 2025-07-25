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
  function main() {
  $ss = ['runtimeFields' => []];
  echo rtrim('Create two fields at runtime: \n'), PHP_EOL;
  $i = 1;
  while ($i <= 2) {
  echo rtrim('  Field #' . _str($i) . ':\n'), PHP_EOL;
  echo rtrim('       Enter name  : '), PHP_EOL;
  $name = trim(fgets(STDIN));
  echo rtrim('       Enter value : '), PHP_EOL;
  $value = trim(fgets(STDIN));
  $fields = $ss['runtimeFields'];
  $fields[$name] = $value;
  $ss['runtimeFields'] = $fields;
  echo rtrim('\n'), PHP_EOL;
  $i = $i + 1;
};
  while (true) {
  echo rtrim('Which field do you want to inspect ? '), PHP_EOL;
  $name = trim(fgets(STDIN));
  if (isset($ss['runtimeFields'][$name])) {
  $value = $ss['runtimeFields'][$name];
  echo rtrim('Its value is \'' . $value . '\''), PHP_EOL;
  return;
} else {
  echo rtrim('There is no field of that name, try again\n'), PHP_EOL;
}
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
