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
  function Box_TellSecret($self) {
  global $funcs, $New, $Count;
  $Contents = $self['Contents'];
  $secret = $self['secret'];
  return $secret;
};
  function newFactory() {
  global $funcs, $New;
  $sn = 0;
  $mochi_New = null;
$mochi_New = function() use (&$New, $sn) {
  $sn = $sn + 1;
  $b = ['secret' => $sn];
  if ($sn == 1) {
  $b['Contents'] = 'rabbit';
} else {
  if ($sn == 2) {
  $b['Contents'] = 'rock';
};
}
  return $b;
};
  $Count = null;
$Count = function() use (&$Count, $sn, $mochi_New) {
  return $sn;
};
  return [$mochi_New, $Count];
};
  $funcs = newFactory();
  $New = $funcs[0];
  $Count = $funcs[1];
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
