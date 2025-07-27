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
  function commatize($n) {
  $s = _str($n);
  $i = strlen($s) - 3;
  while ($i > 0) {
  $s = substr($s, 0, $i - 0) . ',' . substr($s, $i, strlen($s) - $i);
  $i = $i - 3;
};
  return $s;
};
  function main() {
  $data = [['pm' => 10, 'g1' => 4, 's1' => 7, 'g2' => 6, 's2' => 23, 'd' => 16], ['pm' => 100, 'g1' => 14, 's1' => 113, 'g2' => 16, 's2' => 1831, 'd' => 1718], ['pm' => 1000, 'g1' => 14, 's1' => 113, 'g2' => 16, 's2' => 1831, 'd' => 1718], ['pm' => 10000, 'g1' => 36, 's1' => 9551, 'g2' => 38, 's2' => 30593, 'd' => 21042], ['pm' => 100000, 'g1' => 70, 's1' => 173359, 'g2' => 72, 's2' => 31397, 'd' => 141962], ['pm' => 1000000, 'g1' => 100, 's1' => 396733, 'g2' => 102, 's2' => 1444309, 'd' => 1047576], ['pm' => 10000000, 'g1' => 148, 's1' => 2010733, 'g2' => 150, 's2' => 13626257, 'd' => 11615524], ['pm' => 100000000, 'g1' => 198, 's1' => 46006769, 'g2' => 200, 's2' => 378043979, 'd' => 332037210], ['pm' => 1000000000, 'g1' => 276, 's1' => 649580171, 'g2' => 278, 's2' => 4260928601, 'd' => 3611348430], ['pm' => 10000000000, 'g1' => 332, 's1' => 5893180121, 'g2' => 334, 's2' => 30827138509, 'd' => 24933958388], ['pm' => 100000000000, 'g1' => 386, 's1' => 35238645587, 'g2' => 388, 's2' => 156798792223, 'd' => 121560146636]];
  foreach ($data as $entry) {
  $pm = commatize($entry['pm']);
  $line1 = 'Earliest difference > ' . $pm . ' between adjacent prime gap starting primes:';
  echo rtrim($line1), PHP_EOL;
  $line2 = 'Gap ' . _str($entry['g1']) . ' starts at ' . commatize($entry['s1']) . ', gap ' . _str($entry['g2']) . ' starts at ' . commatize($entry['s2']) . ', difference is ' . commatize($entry['d']) . '.';
  echo rtrim($line2), PHP_EOL;
  echo rtrim(''), PHP_EOL;
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
