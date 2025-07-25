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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function fields($s) {
  $words = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == ' ' || $ch == '\n' || $ch == '\t') {
  if (strlen($cur) > 0) {
  $words = array_merge($words, [$cur]);
  $cur = '';
};
} else {
  $cur = $cur . $ch;
}
  $i = $i + 1;
};
  if (strlen($cur) > 0) {
  $words = array_merge($words, [$cur]);
}
  return $words;
};
  function mochi_join($xs, $sep) {
  $res = '';
  $i = 0;
  while ($i < count($xs)) {
  if ($i > 0) {
  $res = $res . $sep;
}
  $res = $res . $xs[$i];
  $i = $i + 1;
};
  return $res;
};
  function numberName($n) {
  $small = ['no', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen'];
  $tens = ['ones', 'ten', 'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety'];
  if ($n < 0) {
  return '';
}
  if ($n < 20) {
  return $small[$n];
}
  if ($n < 100) {
  $t = $tens[intval((_intdiv($n, 10)))];
  $s = $n % 10;
  if ($s > 0) {
  $t = $t . ' ' . $small[$s];
};
  return $t;
}
  return '';
};
  function pluralizeFirst($s, $n) {
  if ($n == 1) {
  return $s;
}
  $w = fields($s);
  if (count($w) > 0) {
  $w[0] = $w[0] . 's';
}
  return mochi_join($w, ' ');
};
  function randInt($seed, $n) {
  $next = ($seed * 1664525 + 1013904223) % 2147483647;
  return $next % $n;
};
  function slur($p, $d) {
  if (strlen($p) <= 2) {
  return $p;
}
  $a = [];
  $i = 1;
  while ($i < strlen($p) - 1) {
  $a = array_merge($a, [substr($p, $i, $i + 1 - $i)]);
  $i = $i + 1;
};
  $idx = count($a) - 1;
  $seed = $d;
  while ($idx >= 1) {
  $seed = ($seed * 1664525 + 1013904223) % 2147483647;
  if ($seed % 100 >= $d) {
  $j = $seed % ($idx + 1);
  $tmp = $a[$idx];
  $a[$idx] = $a[$j];
  $a[$j] = $tmp;
}
  $idx = $idx - 1;
};
  $s = substr($p, 0, 1 - 0);
  $k = 0;
  while ($k < count($a)) {
  $s = $s . $a[$k];
  $k = $k + 1;
};
  $s = $s . substr($p, strlen($p) - 1, strlen($p) - (strlen($p) - 1));
  $w = fields($s);
  return mochi_join($w, ' ');
};
  function main() {
  $i = 99;
  while ($i > 0) {
  echo rtrim(slur(numberName($i), $i) . ' ' . pluralizeFirst(slur('bottle of', $i), $i) . ' ' . slur('beer on the wall', $i)), PHP_EOL;
  echo rtrim(slur(numberName($i), $i) . ' ' . pluralizeFirst(slur('bottle of', $i), $i) . ' ' . slur('beer', $i)), PHP_EOL;
  echo rtrim(slur('take one', $i) . ' ' . slur('down', $i) . ' ' . slur('pass it around', $i)), PHP_EOL;
  echo rtrim(slur(numberName($i - 1), $i) . ' ' . pluralizeFirst(slur('bottle of', $i), $i - 1) . ' ' . slur('beer on the wall', $i)), PHP_EOL;
  $i = $i - 1;
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
