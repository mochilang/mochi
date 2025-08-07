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
  function log2($x) {
  global $text1, $text3;
  $k = 0.0;
  $v = $x;
  while ($v >= 2.0) {
  $v = $v / 2.0;
  $k = $k + 1.0;
};
  while ($v < 1.0) {
  $v = $v * 2.0;
  $k = $k - 1.0;
};
  $z = ($v - 1.0) / ($v + 1.0);
  $zpow = $z;
  $sum = $z;
  $i = 3;
  while ($i <= 9) {
  $zpow = $zpow * $z * $z;
  $sum = $sum + $zpow / (floatval($i));
  $i = $i + 2;
};
  $ln2 = 0.6931471805599453;
  return $k + 2.0 * $sum / $ln2;
};
  function analyze_text($text) {
  global $text1, $text3;
  $single = [];
  $double = [];
  $n = strlen($text);
  if ($n == 0) {
  return ['single' => $single, 'double' => $double];
}
  $last = substr($text, $n - 1, $n - ($n - 1));
  if (array_key_exists($last, $single)) {
  $single[$last] = $single[$last] + 1;
} else {
  $single[$last] = 1;
}
  $first = substr($text, 0, 1 - 0);
  $pair0 = ' ' . $first;
  $double[$pair0] = 1;
  $i = 0;
  while ($i < $n - 1) {
  $ch = substr($text, $i, $i + 1 - $i);
  if (array_key_exists($ch, $single)) {
  $single[$ch] = $single[$ch] + 1;
} else {
  $single[$ch] = 1;
}
  $seq = substr($text, $i, $i + 2 - $i);
  if (array_key_exists($seq, $double)) {
  $double[$seq] = $double[$seq] + 1;
} else {
  $double[$seq] = 1;
}
  $i = $i + 1;
};
  return ['single' => $single, 'double' => $double];
};
  function round_to_int($x) {
  global $text1, $text3;
  if ($x < 0.0) {
  return intval(($x - 0.5));
}
  return intval(($x + 0.5));
};
  function calculate_entropy($text) {
  global $text1, $text3;
  $counts = analyze_text($text);
  $alphas = ' abcdefghijklmnopqrstuvwxyz';
  $total1 = 0;
  foreach (array_keys($counts['single']) as $ch) {
  $total1 = $total1 + $counts['single'][$ch];
};
  $h1 = 0.0;
  $i = 0;
  while ($i < strlen($alphas)) {
  $ch = substr($alphas, $i, $i + 1 - $i);
  if (isset($counts['single'][$ch])) {
  $prob = (floatval($counts['single'][$ch])) / (floatval($total1));
  $h1 = $h1 + $prob * log2($prob);
}
  $i = $i + 1;
};
  $first_entropy = -$h1;
  echo rtrim(_str(round_to_int($first_entropy)) . '.0'), PHP_EOL;
  $total2 = 0;
  foreach (array_keys($counts['double']) as $seq) {
  $total2 = $total2 + $counts['double'][$seq];
};
  $h2 = 0.0;
  $a0 = 0;
  while ($a0 < strlen($alphas)) {
  $ch0 = substr($alphas, $a0, $a0 + 1 - $a0);
  $a1 = 0;
  while ($a1 < strlen($alphas)) {
  $ch1 = substr($alphas, $a1, $a1 + 1 - $a1);
  $seq = $ch0 . $ch1;
  if (isset($counts['double'][$seq])) {
  $prob = (floatval($counts['double'][$seq])) / (floatval($total2));
  $h2 = $h2 + $prob * log2($prob);
}
  $a1 = $a1 + 1;
};
  $a0 = $a0 + 1;
};
  $second_entropy = -$h2;
  echo rtrim(_str(round_to_int($second_entropy)) . '.0'), PHP_EOL;
  $diff = $second_entropy - $first_entropy;
  echo rtrim(_str(round_to_int($diff)) . '.0'), PHP_EOL;
};
  $text1 = 'Behind Winston\'s back the voice ' . 'from the telescreen was still ' . 'babbling and the overfulfilment';
  calculate_entropy($text1);
  $text3 = 'Had repulsive dashwoods suspicion sincerity but advantage now him. ' . 'Remark easily garret nor nay.  Civil those mrs enjoy shy fat merry. ' . 'You greatest jointure saw horrible. He private he on be imagine ' . 'suppose. Fertile beloved evident through no service elderly is. Blind ' . 'there if every no so at. Own neglected you preferred way sincerity ' . 'delivered his attempted. To of message cottage windows do besides ' . 'against uncivil.  Delightful unreserved impossible few estimating ' . 'men favourable see entreaties. She propriety immediate was improving. ' . 'He or entrance humoured likewise moderate. Much nor game son say ' . 'feel. Fat make met can must form into gate. Me we offending prevailed ' . 'discovery.';
  calculate_entropy($text3);
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
