<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function create_queue($capacity) {
  $arr = [];
  $i = 0;
  while ($i < $capacity) {
  $arr = _append($arr, 0);
  $i = $i + 1;
};
  return ['capacity' => $capacity, 'data' => $arr, 'front' => 0, 'rear' => 0, 'size' => 0];
};
  function length($q) {
  return $q['size'];
};
  function is_empty($q) {
  return $q['size'] == 0;
};
  function front($q) {
  if (is_empty($q)) {
  return 0;
}
  return $q['data'][$q['front']];
};
  function enqueue($q, $value) {
  if ($q['size'] >= $q['capacity']) {
  _panic('QUEUE IS FULL');
}
  $arr = $q['data'];
  $arr[$q['rear']] = $value;
  $q['data'] = $arr;
  $q['rear'] = fmod(($q['rear'] + 1), $q['capacity']);
  $q['size'] = $q['size'] + 1;
  return $q;
};
  function dequeue(&$q) {
  if ($q['size'] == 0) {
  _panic('UNDERFLOW');
}
  $value = $q['data'][$q['front']];
  $arr2 = $q['data'];
  $arr2[$q['front']] = 0;
  $q['data'] = $arr2;
  $q['front'] = fmod(($q['front'] + 1), $q['capacity']);
  $q['size'] = $q['size'] - 1;
  return ['queue' => $q, 'value' => $value];
};
  function main() {
  $q = create_queue(5);
  echo rtrim(json_encode(is_empty($q), 1344)), PHP_EOL;
  $q = enqueue($q, 10);
  echo rtrim(json_encode(is_empty($q), 1344)), PHP_EOL;
  $q = enqueue($q, 20);
  $q = enqueue($q, 30);
  echo rtrim(json_encode(front($q), 1344)), PHP_EOL;
  $r = dequeue($q);
  $q = $r['queue'];
  echo rtrim(json_encode($r['value'], 1344)), PHP_EOL;
  echo rtrim(json_encode(front($q), 1344)), PHP_EOL;
  echo rtrim(json_encode(length($q), 1344)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
