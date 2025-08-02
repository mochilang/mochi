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
  function padLeft($n, $width) {
  global $strings, $chars, $i, $j, $c, $ss;
  $s = _str($n);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
};
  function squeeze($s, $ch) {
  global $strings, $chars, $j, $ss;
  $out = '';
  $prev = false;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c == $ch) {
  if (!$prev) {
  $out = $out . $c;
  $prev = true;
};
} else {
  $out = $out . $c;
  $prev = false;
}
  $i = $i + 1;
};
  return $out;
};
  $strings = ['', '"If I were two-faced, would I be wearing this one?" --- Abraham Lincoln ', '..1111111111111111111111111111111111111111111111111111111111111117777888', 'I never give \'em hell, I just tell the truth, and they think it\'s hell. ', '                                                   ---  Harry S Truman  ', 'The better the 4-wheel drive, the further you\'ll be from help when ya get stuck!', 'headmistressship', 'aardvark', '😍😀🙌💃😍😍😍🙌'];
  $chars = [[' '], ['-'], ['7'], ['.'], [' ', '-', 'r'], ['e'], ['s'], ['a'], ['😍']];
  $i = 0;
  while ($i < count($strings)) {
  $j = 0;
  $s = $strings[$i];
  while ($j < count($chars[$i])) {
  $c = $chars[$i][$j];
  $ss = squeeze($s, $c);
  echo rtrim('specified character = \'' . $c . '\''), PHP_EOL;
  echo rtrim('original : length = ' . padLeft(strlen($s), 2) . ', string = «««' . $s . '»»»'), PHP_EOL;
  echo rtrim('squeezed : length = ' . padLeft(strlen($ss), 2) . ', string = «««' . $ss . '»»»'), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $j = $j + 1;
};
  $i = $i + 1;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
