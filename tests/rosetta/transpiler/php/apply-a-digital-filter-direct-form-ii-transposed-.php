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
  function applyFilter($input, $a, $b) {
  global $sig, $res, $k;
  $out = [];
  $scale = 1.0 / $a[0];
  $i = 0;
  while ($i < count($input)) {
  $tmp = 0.0;
  $j = 0;
  while ($j <= $i && $j < count($b)) {
  $tmp = $tmp + $b[$j] * $input[$i - $j];
  $j = $j + 1;
};
  $j = 0;
  while ($j < $i && $j + 1 < count($a)) {
  $tmp = $tmp - $a[$j + 1] * $out[$i - $j - 1];
  $j = $j + 1;
};
  $out = array_merge($out, [$tmp * $scale]);
  $i = $i + 1;
};
  return $out;
};
  $a = [1.0, -0.00000000000000027756, 0.33333333, -0.0000000000000000185];
  $b = [0.16666667, 0.5, 0.5, 0.16666667];
  $sig = [-0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412, -0.662370894973, -1.00700480494, -0.404707073677, 0.800482325044, 0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195, 0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293, 0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589];
  $res = applyFilter($sig, $a, $b);
  $k = 0;
  while ($k < count($res)) {
  echo rtrim(json_encode($res[$k], 1344)), PHP_EOL;
  $k = $k + 1;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
