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
  function getBins($limits, $data) {
  $n = count($limits);
  $bins = [];
  $i = 0;
  while ($i < $n + 1) {
  $bins = array_merge($bins, [0]);
  $i = $i + 1;
};
  $j = 0;
  while ($j < count($data)) {
  $d = $data[$j];
  $index = 0;
  while ($index < count($limits)) {
  if ($d < $limits[$index]) {
  break;
}
  if ($d == $limits[$index]) {
  $index = $index + 1;
  break;
}
  $index = $index + 1;
};
  $bins[$index] = $bins[$index] + 1;
  $j = $j + 1;
};
  return $bins;
};
  function padLeft($n, $width) {
  $s = _str($n);
  $pad = $width - strlen($s);
  $out = '';
  $i = 0;
  while ($i < $pad) {
  $out = $out . ' ';
  $i = $i + 1;
};
  return $out . $s;
};
  function printBins($limits, $bins) {
  $n = count($limits);
  echo rtrim('           < ' . padLeft($limits[0], 3) . ' = ' . padLeft($bins[0], 2)), PHP_EOL;
  $i = 1;
  while ($i < $n) {
  echo rtrim('>= ' . padLeft($limits[$i - 1], 3) . ' and < ' . padLeft($limits[$i], 3) . ' = ' . padLeft($bins[$i], 2)), PHP_EOL;
  $i = $i + 1;
};
  echo rtrim('>= ' . padLeft($limits[$n - 1], 3) . '           = ' . padLeft($bins[$n], 2)), PHP_EOL;
  echo rtrim(''), PHP_EOL;
};
  function main() {
  $limitsList = [[23, 37, 43, 53, 67, 83], [14, 18, 249, 312, 389, 392, 513, 591, 634, 720]];
  $dataList = [[95, 21, 94, 12, 99, 4, 70, 75, 83, 93, 52, 80, 57, 5, 53, 86, 65, 17, 92, 83, 71, 61, 54, 58, 47, 16, 8, 9, 32, 84, 7, 87, 46, 19, 30, 37, 96, 6, 98, 40, 79, 97, 45, 64, 60, 29, 49, 36, 43, 55], [445, 814, 519, 697, 700, 130, 255, 889, 481, 122, 932, 77, 323, 525, 570, 219, 367, 523, 442, 933, 416, 589, 930, 373, 202, 253, 775, 47, 731, 685, 293, 126, 133, 450, 545, 100, 741, 583, 763, 306, 655, 267, 248, 477, 549, 238, 62, 678, 98, 534, 622, 907, 406, 714, 184, 391, 913, 42, 560, 247, 346, 860, 56, 138, 546, 38, 985, 948, 58, 213, 799, 319, 390, 634, 458, 945, 733, 507, 916, 123, 345, 110, 720, 917, 313, 845, 426, 9, 457, 628, 410, 723, 354, 895, 881, 953, 677, 137, 397, 97, 854, 740, 83, 216, 421, 94, 517, 479, 292, 963, 376, 981, 480, 39, 257, 272, 157, 5, 316, 395, 787, 942, 456, 242, 759, 898, 576, 67, 298, 425, 894, 435, 831, 241, 989, 614, 987, 770, 384, 692, 698, 765, 331, 487, 251, 600, 879, 342, 982, 527, 736, 795, 585, 40, 54, 901, 408, 359, 577, 237, 605, 847, 353, 968, 832, 205, 838, 427, 876, 959, 686, 646, 835, 127, 621, 892, 443, 198, 988, 791, 466, 23, 707, 467, 33, 670, 921, 180, 991, 396, 160, 436, 717, 918, 8, 374, 101, 684, 727, 749]];
  $i = 0;
  while ($i < count($limitsList)) {
  echo rtrim('Example ' . _str($i + 1) . '\n'), PHP_EOL;
  $bins = getBins($limitsList[$i], $dataList[$i]);
  printBins($limitsList[$i], $bins);
  $i = $i + 1;
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
