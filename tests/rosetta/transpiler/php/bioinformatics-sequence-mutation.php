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
  function randInt($s, $n) {
  $next = ($s * 1664525 + 1013904223) % 2147483647;
  return [$next, $next % $n];
};
  function padLeft($s, $w) {
  $res = '';
  $n = $w - strlen($s);
  while ($n > 0) {
  $res = $res . ' ';
  $n = $n - 1;
};
  return $res . $s;
};
  function makeSeq($s, $le) {
  $bases = 'ACGT';
  $out = '';
  $i = 0;
  while ($i < $le) {
  $r = randInt($s, 4);
  $s = $r[0];
  $idx = intval($r[1]);
  $out = $out . substr($bases, $idx, $idx + 1 - $idx);
  $i = $i + 1;
};
  return [$s, $out];
};
  function mutate($s, $dna, $w) {
  $bases = 'ACGT';
  $le = strlen($dna);
  $r = randInt($s, $le);
  $s = $r[0];
  $p = intval($r[1]);
  $r = randInt($s, 300);
  $s = $r[0];
  $x = intval($r[1]);
  $arr = [];
  $i = 0;
  while ($i < $le) {
  $arr = array_merge($arr, [substr($dna, $i, $i + 1 - $i)]);
  $i = $i + 1;
};
  if ($x < $w[0]) {
  $r = randInt($s, 4);
  $s = $r[0];
  $idx = intval($r[1]);
  $b = substr($bases, $idx, $idx + 1 - $idx);
  echo rtrim('  Change @' . padLeft(_str($p), 3) . ' \'' . $arr[$p] . '\' to \'' . $b . '\''), PHP_EOL;
  $arr[$p] = $b;
} else {
  if ($x < $w[0] + $w[1]) {
  echo rtrim('  Delete @' . padLeft(_str($p), 3) . ' \'' . $arr[$p] . '\''), PHP_EOL;
  $j = $p;
  while ($j < count($arr) - 1) {
  $arr[$j] = $arr[$j + 1];
  $j = $j + 1;
};
  $arr = array_slice($arr, 0, count($arr) - 1 - 0);
} else {
  $r = randInt($s, 4);
  $s = $r[0];
  $idx2 = intval($r[1]);
  $b = substr($bases, $idx2, $idx2 + 1 - $idx2);
  $arr = array_merge($arr, ['']);
  $j = count($arr) - 1;
  while ($j > $p) {
  $arr[$j] = $arr[$j - 1];
  $j = $j - 1;
};
  echo rtrim('  Insert @' . padLeft(_str($p), 3) . ' \'' . $b . '\''), PHP_EOL;
  $arr[$p] = $b;
};
}
  $out = '';
  $i = 0;
  while ($i < count($arr)) {
  $out = $out . $arr[$i];
  $i = $i + 1;
};
  return [$s, $out];
};
  function prettyPrint($dna, $rowLen) {
  echo rtrim('SEQUENCE:'), PHP_EOL;
  $le = strlen($dna);
  $i = 0;
  while ($i < $le) {
  $k = $i + $rowLen;
  if ($k > $le) {
  $k = $le;
}
  echo rtrim(padLeft(_str($i), 5) . ': ' . substr($dna, $i, $k - $i)), PHP_EOL;
  $i = $i + $rowLen;
};
  $a = 0;
  $c = 0;
  $g = 0;
  $t = 0;
  $idx = 0;
  while ($idx < $le) {
  $ch = substr($dna, $idx, $idx + 1 - $idx);
  if ($ch == 'A') {
  $a = $a + 1;
} else {
  if ($ch == 'C') {
  $c = $c + 1;
} else {
  if ($ch == 'G') {
  $g = $g + 1;
} else {
  if ($ch == 'T') {
  $t = $t + 1;
};
};
};
}
  $idx = $idx + 1;
};
  echo rtrim(''), PHP_EOL;
  echo rtrim('BASE COUNT:'), PHP_EOL;
  echo rtrim('    A: ' . padLeft(_str($a), 3)), PHP_EOL;
  echo rtrim('    C: ' . padLeft(_str($c), 3)), PHP_EOL;
  echo rtrim('    G: ' . padLeft(_str($g), 3)), PHP_EOL;
  echo rtrim('    T: ' . padLeft(_str($t), 3)), PHP_EOL;
  echo rtrim('    ------'), PHP_EOL;
  echo rtrim('    Σ: ' . _str($le)), PHP_EOL;
  echo rtrim('    ======'), PHP_EOL;
};
  function wstring($w) {
  return '  Change: ' . _str($w[0]) . '\n  Delete: ' . _str($w[1]) . '\n  Insert: ' . _str($w[2]) . '\n';
};
  function main() {
  $seed = 1;
  $res = makeSeq($seed, 250);
  $seed = $res[0];
  $dna = strval($res[1]);
  prettyPrint($dna, 50);
  $muts = 10;
  $w = [100, 100, 100];
  echo rtrim('\nWEIGHTS (ex 300):'), PHP_EOL;
  echo rtrim(wstring($w)), PHP_EOL;
  echo rtrim('MUTATIONS (' . _str($muts) . '):'), PHP_EOL;
  $i = 0;
  while ($i < $muts) {
  $res = mutate($seed, $dna, $w);
  $seed = $res[0];
  $dna = strval($res[1]);
  $i = $i + 1;
};
  echo rtrim(''), PHP_EOL;
  prettyPrint($dna, 50);
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
