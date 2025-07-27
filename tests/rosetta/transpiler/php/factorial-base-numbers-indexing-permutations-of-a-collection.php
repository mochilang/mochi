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
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
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
  function split($s, $sep) {
  global $seed;
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
  function mochi_parseIntStr($str) {
  global $seed;
  $i = 0;
  $neg = false;
  if (strlen($str) > 0 && substr($str, 0, 1 - 0) == '-') {
  $neg = true;
  $i = 1;
}
  $n = 0;
  $digits = ['0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9];
  while ($i < strlen($str)) {
  $n = $n * 10 + $digits[substr($str, $i, $i + 1 - $i)];
  $i = $i + 1;
};
  if ($neg) {
  $n = -$n;
}
  return $n;
};
  function joinInts($nums, $sep) {
  global $seed;
  $s = '';
  $i = 0;
  while ($i < count($nums)) {
  if ($i > 0) {
  $s = $s . $sep;
}
  $s = $s . _str($nums[$i]);
  $i = $i + 1;
};
  return $s;
};
  function undot($s) {
  global $seed;
  $parts = explode('.', $s);
  $nums = [];
  foreach ($parts as $p) {
  $nums = array_merge($nums, [parseIntStr($p, 10)]);
};
  return $nums;
};
  function factorial($n) {
  global $seed;
  $f = 1;
  $i = 2;
  while ($i <= $n) {
  $f = $f * $i;
  $i = $i + 1;
};
  return $f;
};
  function genFactBaseNums($size, $countOnly) {
  global $seed;
  $results = [];
  $count = 0;
  $n = 0;
  while (true) {
  $radix = 2;
  $res = [];
  if (!$countOnly) {
  $z = 0;
  while ($z < $size) {
  $res = array_merge($res, [0]);
  $z = $z + 1;
};
}
  $k = $n;
  while ($k > 0) {
  $div = _intdiv($k, $radix);
  $rem = $k % $radix;
  if (!$countOnly && $radix <= $size + 1) {
  $res[$size - $radix + 1] = $rem;
}
  $k = $div;
  $radix = $radix + 1;
};
  if ($radix > $size + 2) {
  break;
}
  $count = $count + 1;
  if (!$countOnly) {
  $results = array_merge($results, [$res]);
}
  $n = $n + 1;
};
  return [$results, $count];
};
  function mapToPerms($factNums) {
  global $seed;
  $perms = [];
  $psize = count($factNums[0]) + 1;
  $start = [];
  $i = 0;
  while ($i < $psize) {
  $start = array_merge($start, [$i]);
  $i = $i + 1;
};
  foreach ($factNums as $fn) {
  $perm = [];
  $j = 0;
  while ($j < count($start)) {
  $perm = array_merge($perm, [$start[$j]]);
  $j = $j + 1;
};
  $m = 0;
  while ($m < count($fn)) {
  $g = $fn[$m];
  if ($g != 0) {
  $first = $m;
  $last = $m + $g;
  $t = 1;
  while ($t <= $g) {
  $temp = $perm[$first];
  $x = $first + 1;
  while ($x <= $last) {
  $perm[$x - 1] = $perm[$x];
  $x = $x + 1;
};
  $perm[$last] = $temp;
  $t = $t + 1;
};
}
  $m = $m + 1;
};
  $perms = array_merge($perms, [$perm]);
};
  return $perms;
};
  $seed = 1;
  function randInt($n) {
  global $seed;
  $seed = ($seed * 1664525 + 1013904223) % 2147483647;
  return $seed % $n;
};
  function main() {
  global $seed;
  $g = genFactBaseNums(3, false);
  $factNums = $g[0];
  $perms = mapToPerms($factNums);
  $i = 0;
  while ($i < _len($factNums)) {
  echo rtrim(joinInts($factNums[$i], '.') . ' -> ' . joinInts($perms[$i], '')), PHP_EOL;
  $i = $i + 1;
};
  $count2 = factorial(11);
  echo rtrim('
Permutations generated = ' . _str($count2)), PHP_EOL;
  echo rtrim('compared to 11! which  = ' . _str(factorial(11))), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $fbn51s = ['39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0', '51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1'];
  $factNums = [undot($fbn51s[0]), undot($fbn51s[1])];
  $perms = mapToPerms($factNums);
  $shoe = 'A♠K♠Q♠J♠T♠9♠8♠7♠6♠5♠4♠3♠2♠A♥K♥Q♥J♥T♥9♥8♥7♥6♥5♥4♥3♥2♥A♦K♦Q♦J♦T♦9♦8♦7♦6♦5♦4♦3♦2♦A♣K♣Q♣J♣T♣9♣8♣7♣6♣5♣4♣3♣2♣';
  $cards = [];
  $i = 0;
  while ($i < 52) {
  $card = substr($shoe, 2 * $i, 2 * $i + 2 - 2 * $i);
  if (substr($card, 0, 1 - 0) == 'T') {
  $card = '10' . substr($card, 1, 2 - 1);
}
  $cards = array_merge($cards, [$card]);
  $i = $i + 1;
};
  $i = 0;
  while ($i < count($fbn51s)) {
  echo rtrim($fbn51s[$i]), PHP_EOL;
  $perm = $perms[$i];
  $j = 0;
  $line = '';
  while ($j < count($perm)) {
  $line = $line . $cards[$perm[$j]];
  $j = $j + 1;
};
  echo rtrim($line . '
'), PHP_EOL;
  $i = $i + 1;
};
  $fbn51 = [];
  $i = 0;
  while ($i < 51) {
  $fbn51 = array_merge($fbn51, [randInt(52 - $i)]);
  $i = $i + 1;
};
  echo rtrim(joinInts($fbn51, '.')), PHP_EOL;
  $perms = mapToPerms([$fbn51]);
  $line = '';
  $i = 0;
  while ($i < count($perms[0])) {
  $line = $line . $cards[$perms[0][$i]];
  $i = $i + 1;
};
  echo rtrim($line), PHP_EOL;
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
