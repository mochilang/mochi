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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $small = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen'];
  $tens = ['', '', 'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety'];
  $smallOrd = ['zeroth', 'first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 'eighth', 'ninth', 'tenth', 'eleventh', 'twelfth', 'thirteenth', 'fourteenth', 'fifteenth', 'sixteenth', 'seventeenth', 'eighteenth', 'nineteenth'];
  $tensOrd = ['', '', 'twentieth', 'thirtieth', 'fortieth', 'fiftieth', 'sixtieth', 'seventieth', 'eightieth', 'ninetieth'];
  function say($n) {
  global $small, $tens, $smallOrd, $tensOrd, $words, $idx;
  if ($n < 20) {
  return $small[$n];
}
  if ($n < 100) {
  $res = $tens[_intdiv($n, 10)];
  $m = $n % 10;
  if ($m != 0) {
  $res = $res . '-' . $small[$m];
};
  return $res;
}
  if ($n < 1000) {
  $res = say(_intdiv($n, 100)) . ' hundred';
  $m = $n % 100;
  if ($m != 0) {
  $res = $res . ' ' . say($m);
};
  return $res;
}
  if ($n < 1000000) {
  $res = say(_intdiv($n, 1000)) . ' thousand';
  $m = $n % 1000;
  if ($m != 0) {
  $res = $res . ' ' . say($m);
};
  return $res;
}
  $res = say(_intdiv($n, 1000000)) . ' million';
  $m = $n % 1000000;
  if ($m != 0) {
  $res = $res . ' ' . say($m);
}
  return $res;
};
  function sayOrdinal($n) {
  global $small, $tens, $smallOrd, $tensOrd, $words, $idx;
  if ($n < 20) {
  return $smallOrd[$n];
}
  if ($n < 100) {
  if ($n % 10 == 0) {
  return $tensOrd[_intdiv($n, 10)];
};
  return say($n - $n % 10) . '-' . $smallOrd[$n % 10];
}
  if ($n < 1000) {
  if ($n % 100 == 0) {
  return say(_intdiv($n, 100)) . ' hundredth';
};
  return say(_intdiv($n, 100)) . ' hundred ' . sayOrdinal($n % 100);
}
  if ($n < 1000000) {
  if ($n % 1000 == 0) {
  return say(_intdiv($n, 1000)) . ' thousandth';
};
  return say(_intdiv($n, 1000)) . ' thousand ' . sayOrdinal($n % 1000);
}
  if ($n % 1000000 == 0) {
  return say(_intdiv($n, 1000000)) . ' millionth';
}
  return say(_intdiv($n, 1000000)) . ' million ' . sayOrdinal($n % 1000000);
};
  function split($s, $sep) {
  global $small, $tens, $smallOrd, $tensOrd, $words, $idx;
  $parts = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  if (strlen($sep) > 0 && $i + strlen($sep) <= strlen($s) && substr($s, $i, $i + strlen($sep) - $i) == $sep) {
  $parts = array_merge($parts, [$cur]);
  $cur = '';
  $i = $i + strlen($sep);
} else {
  $cur = $cur . substr($s, $i, $i + 1 - $i);
  $i = $i + 1;
}
};
  $parts = array_merge($parts, [$cur]);
  return $parts;
};
  function countLetters($s) {
  global $small, $tens, $smallOrd, $tensOrd, $words, $idx;
  $cnt = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch >= 'A' && $ch <= 'Z' || $ch >= 'a' && $ch <= 'z') {
  $cnt = $cnt + 1;
}
  $i = $i + 1;
};
  return $cnt;
};
  $words = ['Four', 'is', 'the', 'number', 'of', 'letters', 'in', 'the', 'first', 'word', 'of', 'this', 'sentence,'];
  $idx = 0;
  function wordLen($w) {
  global $small, $tens, $smallOrd, $tensOrd, $words, $idx;
  while (count($words) < $w) {
  $idx = $idx + 1;
  $n = countLetters($words[$idx]);
  $parts = explode(' ', say($n));
  $j = 0;
  while ($j < count($parts)) {
  $words = array_merge($words, [$parts[$j]]);
  $j = $j + 1;
};
  $words = array_merge($words, ['in']);
  $words = array_merge($words, ['the']);
  $parts = explode(' ', sayOrdinal($idx + 1) . ',');
  $j = 0;
  while ($j < count($parts)) {
  $words = array_merge($words, [$parts[$j]]);
  $j = $j + 1;
};
};
  $word = $words[$w - 1];
  return [$word, countLetters($word)];
};
  function totalLength() {
  global $small, $tens, $smallOrd, $tensOrd, $words, $idx;
  $tot = 0;
  $i = 0;
  while ($i < count($words)) {
  $tot = $tot + strlen($words[$i]);
  if ($i < count($words) - 1) {
  $tot = $tot + 1;
}
  $i = $i + 1;
};
  return $tot;
};
  function pad($n, $width) {
  global $small, $tens, $smallOrd, $tensOrd, $words, $idx;
  $s = _str($n);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
};
  function main() {
  global $small, $tens, $smallOrd, $tensOrd, $words, $idx;
  echo rtrim('The lengths of the first 201 words are:'), PHP_EOL;
  $line = '';
  $i = 1;
  while ($i <= 201) {
  if ($i % 25 == 1) {
  if ($i != 1) {
  echo rtrim($line), PHP_EOL;
};
  $line = pad($i, 3) . ':';
}
  $r = wordLen($i);
  $n = $r[1];
  $line = $line . ' ' . pad($n, 2);
  $i = $i + 1;
};
  echo rtrim($line), PHP_EOL;
  echo rtrim('Length of sentence so far: ' . _str(totalLength())), PHP_EOL;
  foreach ([1000, 10000, 100000, 1000000, 10000000] as $n) {
  $r = wordLen($n);
  $w = $r[0];
  $l = $r[1];
  echo rtrim('Word ' . pad($n, 8) . ' is "' . $w . '", with ' . _str($l) . ' letters.  Length of sentence so far: ' . _str(totalLength())), PHP_EOL;
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
