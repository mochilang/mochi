<?php
ini_set('memory_limit', '-1');
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
$animal = ['Rat', 'Ox', 'Tiger', 'Rabbit', 'Dragon', 'Snake', 'Horse', 'Goat', 'Monkey', 'Rooster', 'Dog', 'Pig'];
$yinYang = ['Yang', 'Yin'];
$element = ['Wood', 'Fire', 'Earth', 'Metal', 'Water'];
$stemChArr = ['甲', '乙', '丙', '丁', '戊', '己', '庚', '辛', '壬', '癸'];
$branchChArr = ['子', '丑', '寅', '卯', '辰', '巳', '午', '未', '申', '酉', '戌', '亥'];
function cz($yr, $animal, $yinYang, $element, $sc, $bc) {
  global $stemChArr, $branchChArr, $r;
  $y = $yr - 4;
  $stem = $y % 10;
  $branch = $y % 12;
  $sb = $sc[$stem] . $bc[$branch];
  return ['animal' => strval($animal[$branch]), 'yinYang' => strval($yinYang[$stem % 2]), 'element' => strval($element[intval((intdiv($stem, 2)))]), 'stemBranch' => $sb, 'cycle' => $y % 60 + 1];
}
foreach ([1935, 1938, 1968, 1972, 1976] as $yr) {
  $r = cz($yr, $animal, $yinYang, $element, $stemChArr, $branchChArr);
  echo rtrim(_str($yr) . ': ' . $r['element'] . ' ' . $r['animal'] . ', ' . $r['yinYang'] . ', Cycle year ' . _str($r['cycle']) . ' ' . $r['stemBranch']), PHP_EOL;
}
