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
  global $source;
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
  function entropy($data) {
  global $source;
  if ($data == '') {
  return 0.0;
}
  $counts = [];
  $i = 0;
  while ($i < strlen($data)) {
  $ch = substr($data, $i, $i + 1 - $i);
  if (array_key_exists($ch, $counts)) {
  $counts[$ch] = $counts[$ch] + 1;
} else {
  $counts[$ch] = 1;
}
  $i = $i + 1;
};
  $e = 0.0;
  $l = floatval(strlen($data));
  foreach (array_keys($counts) as $ch) {
  $px = (floatval($counts[$ch])) / $l;
  if ($px > 0.0) {
  $e = $e - $px * log2($px);
}
};
  return $e;
};
  $source = '// Mochi translation of the Rosetta "Entropy-Narcissist" task
' . '// Simplified to compute the entropy of this source string

' . 'fun log2(x: float): float {
' . '  var k = 0.0
' . '  var v = x
' . '  while v >= 2.0 {
' . '    v = v / 2.0
' . '    k = k + 1.0
' . '  }
' . '  while v < 1.0 {
' . '    v = v * 2.0
' . '    k = k - 1.0
' . '  }
' . '  let z = (v - 1.0) / (v + 1.0)
' . '  var zpow = z
' . '  var sum = z
' . '  var i = 3
' . '  while i <= 9 {
' . '    zpow = zpow * z * z
' . '    sum = sum + zpow / (i as float)
' . '    i = i + 2
' . '  }
' . '  let ln2 = 0.6931471805599453
' . '  return k + 2.0 * sum / ln2
' . '}

' . 'fun entropy(data: string): float {
' . '  if data == "" { return 0.0 }
' . '  var counts: map<string,int> = {}
' . '  var i = 0
' . '  while i < len(data) {
' . '    let ch = substring(data, i, i+1)
' . '    if ch in counts {
' . '      counts[ch] = counts[ch] + 1
' . '    } else {
' . '      counts[ch] = 1
' . '    }
' . '    i = i + 1
' . '  }
' . '  var e = 0.0
' . '  let l = len(data) as float
' . '  for ch in counts {
' . '    let px = (counts[ch] as float) / l
' . '    if px > 0.0 {
' . '      e = e - px * log2(px)
' . '    }
' . '  }
' . '  return e
' . '}

' . '// Store the program source as a string constant
' . 'let source = ... // truncated in actual source
' . '
fun main() {
' . '  print("Source file entropy: " + str(entropy(source)))
' . '}

' . 'main()
';
  function main() {
  global $source;
  echo rtrim('Source file entropy: ' . _str(entropy($source))), PHP_EOL;
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
