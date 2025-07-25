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
  function fields($s) {
  $res = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c == ' ') {
  if (strlen($cur) > 0) {
  $res = array_merge($res, [$cur]);
  $cur = '';
};
} else {
  $cur = $cur . $c;
}
  $i = $i + 1;
};
  if (strlen($cur) > 0) {
  $res = array_merge($res, [$cur]);
}
  return $res;
};
  function canSpell($word, $blks) {
  if (strlen($word) == 0) {
  return true;
}
  $c = strtolower(substr($word, 0, 1 - 0));
  $i = 0;
  while ($i < count($blks)) {
  $b = $blks[$i];
  if ($c == strtolower(substr($b, 0, 1 - 0)) || $c == strtolower(substr($b, 1, 2 - 1))) {
  $rest = [];
  $j = 0;
  while ($j < count($blks)) {
  if ($j != $i) {
  $rest = array_merge($rest, [$blks[$j]]);
}
  $j = $j + 1;
};
  if (canSpell(substr($word, 1), $rest)) {
  return true;
};
}
  $i = $i + 1;
};
  return false;
};
  function newSpeller($blocks) {
  $bl = fields($blocks);
  return function($w) use ($blocks, $bl) {
  return canSpell($w, $bl);
};
};
  function main() {
  $sp = newSpeller('BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM');
  foreach (['A', 'BARK', 'BOOK', 'TREAT', 'COMMON', 'SQUAD', 'CONFUSE'] as $word) {
  echo rtrim($word . ' ' . _str($sp($word))), PHP_EOL;
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
