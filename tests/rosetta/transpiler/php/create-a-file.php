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
  function createFile(&$fs, $fn) {
  if (array_key_exists($fn, $fs)) {
  echo rtrim('open ' . $fn . ': file exists'), PHP_EOL;
} else {
  $fs[$fn] = false;
  echo rtrim('file ' . $fn . ' created!'), PHP_EOL;
}
};
  function createDir(&$fs, $dn) {
  if (array_key_exists($dn, $fs)) {
  echo rtrim('mkdir ' . $dn . ': file exists'), PHP_EOL;
} else {
  $fs[$dn] = true;
  echo rtrim('directory ' . $dn . ' created!'), PHP_EOL;
}
};
  function main() {
  $fs = [];
  $fs['docs'] = true;
  createFile($fs, 'input.txt');
  createFile($fs, '/input.txt');
  createDir($fs, 'docs');
  createDir($fs, '/docs');
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
