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
  function padLeft($s, $w) {
  global $dna, $le, $i, $k, $a, $c, $g, $t, $idx, $ch;
  $res = '';
  $n = $w - strlen($s);
  while ($n > 0) {
  $res = $res . ' ';
  $n = $n - 1;
};
  return $res . $s;
};
  $dna = '' . 'CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATG' . 'CTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTG' . 'AGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGAT' . 'GGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT' . 'CGATTCTGCTTATAACACTATGTTCTTATGAAATGGATGTTCTGAGTTGG' . 'TCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA' . 'TTTAATTTTTCTATATAGCGATCTGTATTTAAGCAATTCATTTAGGTTAT' . 'CGCCGCGATGCTCGGTTCGGACCGCCAAGCATCTGGCTCCACTGCTAGTG' . 'TCCTAAATTTGAATGGCAAACACAAATAAGATTTAGCAATTCGTGTAGAC' . 'GACCGGGGACTTGCATGATGGGAGCAGCTTTGTTAAACTACGAACGTAAT';
  echo rtrim('SEQUENCE:'), PHP_EOL;
  $le = strlen($dna);
  $i = 0;
  while ($i < $le) {
  $k = $i + 50;
  if ($k > $le) {
  $k = $le;
}
  echo rtrim(padLeft(_str($i), 5) . ': ' . substr($dna, $i, $k - $i)), PHP_EOL;
  $i = $i + 50;
}
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
}
  echo rtrim(''), PHP_EOL;
  echo rtrim('BASE COUNT:'), PHP_EOL;
  echo rtrim('    A: ' . padLeft(_str($a), 3)), PHP_EOL;
  echo rtrim('    C: ' . padLeft(_str($c), 3)), PHP_EOL;
  echo rtrim('    G: ' . padLeft(_str($g), 3)), PHP_EOL;
  echo rtrim('    T: ' . padLeft(_str($t), 3)), PHP_EOL;
  echo rtrim('    ------'), PHP_EOL;
  echo rtrim('    Σ: ' . _str($le)), PHP_EOL;
  echo rtrim('    ======'), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
