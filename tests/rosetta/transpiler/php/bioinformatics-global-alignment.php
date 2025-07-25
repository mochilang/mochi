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
  $res = '';
  $n = $w - strlen($s);
  while ($n > 0) {
  $res = $res . ' ';
  $n = $n - 1;
};
  return $res . $s;
};
  function indexOfFrom($s, $ch, $start) {
  $i = $start;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function containsStr($s, $sub) {
  $i = 0;
  $sl = strlen($s);
  $subl = strlen($sub);
  while ($i <= $sl - $subl) {
  if (substr($s, $i, $i + $subl - $i) == $sub) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function distinct($slist) {
  $res = [];
  foreach ($slist as $s) {
  $found = false;
  foreach ($res as $r) {
  if ($r == $s) {
  $found = true;
  break;
}
};
  if (!$found) {
  $res = array_merge($res, [$s]);
}
};
  return $res;
};
  function permutations($xs) {
  if (count($xs) <= 1) {
  return [$xs];
}
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  $rest = [];
  $j = 0;
  while ($j < count($xs)) {
  if ($j != $i) {
  $rest = array_merge($rest, [$xs[$j]]);
}
  $j = $j + 1;
};
  $subs = permutations($rest);
  foreach ($subs as $p) {
  $perm = [$xs[$i]];
  $k = 0;
  while ($k < count($p)) {
  $perm = array_merge($perm, [$p[$k]]);
  $k = $k + 1;
};
  $res = array_merge($res, [$perm]);
};
  $i = $i + 1;
};
  return $res;
};
  function headTailOverlap($s1, $s2) {
  $start = 0;
  while (true) {
  $ix = indexOfFrom($s1, substr($s2, 0, 1 - 0), $start);
  if ($ix == 0 - 1) {
  return 0;
}
  $start = $ix;
  if (substr($s2, 0, strlen($s1) - $start - 0) == substr($s1, $start, strlen($s1) - $start)) {
  return strlen($s1) - $start;
}
  $start = $start + 1;
};
};
  function deduplicate($slist) {
  $arr = distinct($slist);
  $filtered = [];
  $i = 0;
  while ($i < count($arr)) {
  $s1 = $arr[$i];
  $within = false;
  $j = 0;
  while ($j < count($arr)) {
  if ($j != $i && containsStr($arr[$j], $s1)) {
  $within = true;
  break;
}
  $j = $j + 1;
};
  if (!$within) {
  $filtered = array_merge($filtered, [$s1]);
}
  $i = $i + 1;
};
  return $filtered;
};
  function joinAll($ss) {
  $out = '';
  foreach ($ss as $s) {
  $out = $out . $s;
};
  return $out;
};
  function shortestCommonSuperstring($slist) {
  $ss = deduplicate($slist);
  $shortest = joinAll($ss);
  $perms = permutations($ss);
  $idx = 0;
  while ($idx < count($perms)) {
  $perm = $perms[$idx];
  $sup = $perm[0];
  $i = 0;
  while ($i < count($ss) - 1) {
  $ov = headTailOverlap($perm[$i], $perm[$i + 1]);
  $sup = $sup . substr($perm[$i + 1], $ov, strlen($perm[$i + 1]) - $ov);
  $i = $i + 1;
};
  if (strlen($sup) < strlen($shortest)) {
  $shortest = $sup;
}
  $idx = $idx + 1;
};
  return $shortest;
};
  function printCounts($seq) {
  $a = 0;
  $c = 0;
  $g = 0;
  $t = 0;
  $i = 0;
  while ($i < strlen($seq)) {
  $ch = substr($seq, $i, $i + 1 - $i);
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
  $i = $i + 1;
};
  $total = strlen($seq);
  echo rtrim('\nNucleotide counts for ' . $seq . ':\n'), PHP_EOL;
  echo rtrim(padLeft('A', 10) . padLeft(_str($a), 12)), PHP_EOL;
  echo rtrim(padLeft('C', 10) . padLeft(_str($c), 12)), PHP_EOL;
  echo rtrim(padLeft('G', 10) . padLeft(_str($g), 12)), PHP_EOL;
  echo rtrim(padLeft('T', 10) . padLeft(_str($t), 12)), PHP_EOL;
  echo rtrim(padLeft('Other', 10) . padLeft(_str($total - ($a + $c + $g + $t)), 12)), PHP_EOL;
  echo rtrim('  ____________________'), PHP_EOL;
  echo rtrim(padLeft('Total length', 14) . padLeft(_str($total), 8)), PHP_EOL;
};
  function main() {
  $tests = [['TA', 'AAG', 'TA', 'GAA', 'TA'], ['CATTAGGG', 'ATTAG', 'GGG', 'TA'], ['AAGAUGGA', 'GGAGCGCAUC', 'AUCGCAAUAAGGA'], ['ATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTAT', 'GGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGT', 'CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA', 'TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC', 'AACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT', 'GCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTC', 'CGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATTCTGCTTATAACACTATGTTCT', 'TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC', 'CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATGCTCGTGC', 'GATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATT', 'TTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC', 'CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA', 'TCTCTTAAACTCCTGCTAAATGCTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGA']];
  foreach ($tests as $seqs) {
  $scs = shortestCommonSuperstring($seqs);
  printCounts($scs);
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
