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
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
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
  $pkg_dog = 'Salt';
  $Dog = 'Pepper';
  $pkg_DOG = 'Mustard';
  $packageSees = null;
$packageSees = function($d1, $d2, $d3) use (&$packageSees, $pkg_dog, $Dog, $pkg_DOG) {
  echo rtrim('Package sees: ' . $d1 . ' ' . $d2 . ' ' . $d3), PHP_EOL;
  return ['pkg_dog' => true, 'Dog' => true, 'pkg_DOG' => true];
};
  $d = $packageSees($pkg_dog, $Dog, $pkg_DOG);
  echo rtrim('There are ' . _str(_len($d)) . ' dogs.
'), PHP_EOL;
  $dog = 'Benjamin';
  $d = $packageSees($pkg_dog, $Dog, $pkg_DOG);
  echo rtrim('Main sees:   ' . $dog . ' ' . $Dog . ' ' . $pkg_DOG), PHP_EOL;
  $d['dog'] = true;
  $d['Dog'] = true;
  $d['pkg_DOG'] = true;
  echo rtrim('There are ' . _str(_len($d)) . ' dogs.
'), PHP_EOL;
  $Dog = 'Samba';
  $d = $packageSees($pkg_dog, $Dog, $pkg_DOG);
  echo rtrim('Main sees:   ' . $dog . ' ' . $Dog . ' ' . $pkg_DOG), PHP_EOL;
  $d['dog'] = true;
  $d['Dog'] = true;
  $d['pkg_DOG'] = true;
  echo rtrim('There are ' . _str(_len($d)) . ' dogs.
'), PHP_EOL;
  $DOG = 'Bernie';
  $d = $packageSees($pkg_dog, $Dog, $pkg_DOG);
  echo rtrim('Main sees:   ' . $dog . ' ' . $Dog . ' ' . $DOG), PHP_EOL;
  $d['dog'] = true;
  $d['Dog'] = true;
  $d['pkg_DOG'] = true;
  $d['DOG'] = true;
  echo rtrim('There are ' . _str(_len($d)) . ' dogs.'), PHP_EOL;
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
