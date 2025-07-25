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
  function beastKind($b) {
  return (function($__v) {
  if ($__v['__tag'] === "Dog") {
    $k = $__v["kind"];
    return $k;
  } elseif ($__v['__tag'] === "Cat") {
    $k = $__v["kind"];
    return $k;
  }
})($b);
};
  function beastName($b) {
  return (function($__v) {
  if ($__v['__tag'] === "Dog") {
    $n = $__v["name"];
    return $n;
  } elseif ($__v['__tag'] === "Cat") {
    $n = $__v["name"];
    return $n;
  }
})($b);
};
  function beastCry($b) {
  return (function($__v) {
  if ($__v['__tag'] === "Dog") {
    return 'Woof';
  } elseif ($__v['__tag'] === "Cat") {
    return 'Meow';
  }
})($b);
};
  function bprint($b) {
  echo rtrim(beastName($b) . ', who\'s a ' . beastKind($b) . ', cries: "' . beastCry($b) . '".'), PHP_EOL;
};
  function main() {
  $d = ['__tag' => 'Dog', 'kind' => 'labrador', 'name' => 'Max'];
  $c = ['__tag' => 'Cat', 'kind' => 'siamese', 'name' => 'Sammy'];
  bprint($d);
  bprint($c);
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
