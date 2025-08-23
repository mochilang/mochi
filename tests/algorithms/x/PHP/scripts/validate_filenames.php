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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_indexOf($s, $sub) {
  $n = strlen($s);
  $m = strlen($sub);
  $i = 0;
  while ($i <= $n - $m) {
  if (substr($s, $i, $i + $m - $i) == $sub) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function mochi_contains($s, $sub) {
  return _indexof($s, $sub) >= 0;
};
  function validate($files) {
  $upper = [];
  $space = [];
  $hyphen = [];
  $nodir = [];
  foreach ($files as $f) {
  if ($f != strtolower($f)) {
  $upper = _append($upper, $f);
}
  if (mochi_contains($f, ' ')) {
  $space = _append($space, $f);
}
  if (mochi_contains($f, '-') && mochi_contains($f, '/site-packages/') == false) {
  $hyphen = _append($hyphen, $f);
}
  if (!mochi_contains($f, '/')) {
  $nodir = _append($nodir, $f);
}
};
  if (count($upper) > 0) {
  echo rtrim(_str(count($upper)) . ' files contain uppercase characters:'), PHP_EOL;
  foreach ($upper as $f) {
  echo rtrim($f), PHP_EOL;
};
  echo rtrim(''), PHP_EOL;
}
  if (count($space) > 0) {
  echo rtrim(_str(count($space)) . ' files contain space characters:'), PHP_EOL;
  foreach ($space as $f) {
  echo rtrim($f), PHP_EOL;
};
  echo rtrim(''), PHP_EOL;
}
  if (count($hyphen) > 0) {
  echo rtrim(_str(count($hyphen)) . ' files contain hyphen characters:'), PHP_EOL;
  foreach ($hyphen as $f) {
  echo rtrim($f), PHP_EOL;
};
  echo rtrim(''), PHP_EOL;
}
  if (count($nodir) > 0) {
  echo rtrim(_str(count($nodir)) . ' files are not in a directory:'), PHP_EOL;
  foreach ($nodir as $f) {
  echo rtrim($f), PHP_EOL;
};
  echo rtrim(''), PHP_EOL;
}
  return count($upper) + count($space) + count($hyphen) + count($nodir);
};
  function main() {
  $files = ['scripts/Validate_filenames.py', 'good/file.txt', 'bad file.txt', '/site-packages/pkg-name.py', 'nopath', 'src/hyphen-name.py'];
  $bad = validate($files);
  echo rtrim(_str($bad)), PHP_EOL;
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
