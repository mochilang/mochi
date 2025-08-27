<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $digits = ['0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9];
  function parseInt($s) {
  global $digits;
  $i = 0;
  $n = 0;
  while ($i < strlen($s)) {
  $n = $n * 10 + (intval($digits[$s[$i]]));
  $i = $i + 1;
};
  return $n;
};
  function mochi_split($s) {
  global $digits;
  $parts = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == ' ') {
  if (strlen($cur) > 0) {
  $parts = _append($parts, $cur);
  $cur = '';
};
} else {
  $cur = $cur . $ch;
}
  $i = $i + 1;
};
  if (strlen($cur) > 0) {
  $parts = _append($parts, $cur);
}
  return $parts;
};
  function absf($x) {
  global $digits;
  if ($x < 0.0) {
  return -$x;
} else {
  return $x;
}
};
  function mochi_sqrt($x) {
  global $digits;
  if ($x <= 0.0) {
  return 0.0;
}
  $r = $x;
  $prev = 0.0;
  while (absf($r - $prev) > 0.000000000001) {
  $prev = $r;
  $r = ($r + $x / $r) / 2.0;
};
  return $r;
};
  function makeBoolGrid($P, $Q) {
  global $digits;
  $g = [];
  $i = 0;
  while ($i < $P) {
  $row = [];
  $j = 0;
  while ($j < $Q) {
  $row = _append($row, false);
  $j = $j + 1;
};
  $g = _append($g, $row);
  $i = $i + 1;
};
  return $g;
};
  function visible($grid, $P, $Q, $R, $C, $BR, $BC) {
  global $digits;
  $X1 = (floatval($C)) - 0.5;
  $Y1 = (floatval($R)) - 0.5;
  $Z1 = (floatval($grid[intval(($R - 1))][intval(($C - 1))])) + 0.5;
  $X2 = (floatval($BC)) - 0.5;
  $Y2 = (floatval($BR)) - 0.5;
  $Z2 = (floatval($grid[intval(($BR - 1))][intval(($BC - 1))])) + 0.5;
  $Dx = $X2 - $X1;
  $Dy = $Y2 - $Y1;
  $Dz = $Z2 - $Z1;
  $dist = mochi_sqrt($Dx * $Dx + $Dy * $Dy + $Dz * $Dz);
  $steps = (intval(($dist * 20.0))) + 1;
  $stepT = 1.0 / (floatval($steps));
  $i = 1;
  while ($i < $steps) {
  $t = $stepT * (floatval($i));
  $X = $X1 + $Dx * $t;
  $Y = $Y1 + $Dy * $t;
  $Z = $Z1 + $Dz * $t;
  $rIdx = ((intval($Y))) + 1;
  $cIdx = ((intval($X))) + 1;
  if ($rIdx < 1 || $rIdx > $P || $cIdx < 1 || $cIdx > $Q) {
  return false;
}
  $H = floatval($grid[intval(($rIdx - 1))][intval(($cIdx - 1))]);
  if ($Z <= $H) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function computeVis($grid, $P, $Q, $BR, $BC) {
  global $digits;
  $vis = makeBoolGrid($P, $Q);
  $r = 1;
  while ($r <= $P) {
  $c = 1;
  while ($c <= $Q) {
  $vis[intval(($r - 1))][intval(($c - 1))] = visible($grid, $P, $Q, $r, $c, $BR, $BC);
  $c = $c + 1;
};
  $r = $r + 1;
};
  return $vis;
};
  function bfs($grid, $P, $Q, $R1, $C1, $R2, $C2) {
  global $digits;
  $vis1 = computeVis($grid, $P, $Q, $R1, $C1);
  $vis2 = computeVis($grid, $P, $Q, $R2, $C2);
  $visited = makeBoolGrid($P, $Q);
  $qR = [];
  $qC = [];
  $qD = [];
  $qR = _append($qR, $R1);
  $qC = _append($qC, $C1);
  $qD = _append($qD, 0);
  $visited[intval(($R1 - 1))][intval(($C1 - 1))] = true;
  $head = 0;
  while ($head < count($qR)) {
  $r = $qR[$head];
  $c = $qC[$head];
  $d = $qD[$head];
  if ($r == $R2 && $c == $C2) {
  return $d;
}
  $hr = $grid[intval(($r - 1))][intval(($c - 1))];
  $idx = 0;
  while ($idx < 4) {
  $nr = $r;
  $nc = $c;
  if ($idx == 0) {
  $nr = $nr - 1;
}
  if ($idx == 1) {
  $nr = $nr + 1;
}
  if ($idx == 2) {
  $nc = $nc - 1;
}
  if ($idx == 3) {
  $nc = $nc + 1;
}
  if ($nr >= 1 && $nr <= $P && $nc >= 1 && $nc <= $Q) {
  if (!$visited[intval(($nr - 1))][intval(($nc - 1))]) {
  $hn = $grid[intval(($nr - 1))][intval(($nc - 1))];
  $diff = $hn - $hr;
  if ($diff <= 1 && $diff >= 0 - 3) {
  if ($vis1[intval(($nr - 1))][intval(($nc - 1))] || $vis2[intval(($nr - 1))][intval(($nc - 1))]) {
  $visited[intval(($nr - 1))][intval(($nc - 1))] = true;
  $qR = _append($qR, $nr);
  $qC = _append($qC, $nc);
  $qD = _append($qD, $d + 1);
};
};
};
}
  $idx = $idx + 1;
};
  $head = $head + 1;
};
  return -1;
};
  function main() {
  global $digits;
  $tLine = trim(fgets(STDIN));
  if ($tLine == '') {
  return;
}
  $t = parseInt($tLine);
  $case = 0;
  while ($case < $t) {
  $line = trim(fgets(STDIN));
  while ($line == '') {
  $line = trim(fgets(STDIN));
};
  $pq = mochi_split($line);
  $P = parseInt($pq[0]);
  $Q = parseInt($pq[1]);
  $grid = [];
  $r = 0;
  while ($r < $P) {
  $rowParts = mochi_split(trim(fgets(STDIN)));
  $row = [];
  $c = 0;
  while ($c < $Q) {
  $row = _append($row, parseInt($rowParts[$c]));
  $c = $c + 1;
};
  $grid = _append($grid, $row);
  $r = $r + 1;
};
  $coords = mochi_split(trim(fgets(STDIN)));
  $R1 = parseInt($coords[0]);
  $C1 = parseInt($coords[1]);
  $R2 = parseInt($coords[2]);
  $C2 = parseInt($coords[3]);
  $res = bfs($grid, $P, $Q, $R1, $C1, $R2, $C2);
  if ($res < 0) {
  echo rtrim('Mission impossible!'), PHP_EOL;
} else {
  echo rtrim('The shortest path is ' . _str($res) . ' steps long.'), PHP_EOL;
}
  $case = $case + 1;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
