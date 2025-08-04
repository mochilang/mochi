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
$__start_mem = memory_get_usage();
$__start = _now();
  function sqrtApprox($x) {
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function powf($base, $exp) {
  $result = 1.0;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function normalize($v) {
  $len = sqrtApprox($v['x'] * $v['x'] + $v['y'] * $v['y'] + $v['z'] * $v['z']);
  return ['x' => $v['x'] / $len, 'y' => $v['y'] / $len, 'z' => $v['z'] / $len];
};
  function dot($a, $b) {
  $d = $a['x'] * $b['x'] + $a['y'] * $b['y'] + $a['z'] * $b['z'];
  if ($d < 0.0) {
  return -$d;
}
  return 0.0;
};
  function hitSphere($s, $x, $y) {
  $dx = $x - $s['cx'];
  $dy = $y - $s['cy'];
  $zsq = $s['r'] * $s['r'] - ($dx * $dx + $dy * $dy);
  if ($zsq < 0.0) {
  return ['hit' => false];
}
  $z = sqrtApprox($zsq);
  return ['hit' => true, 'z1' => $s['cz'] - $z, 'z2' => $s['cz'] + $z];
};
  function main() {
  $shades = '.:!*oe&#%@';
  $light = normalize(['x' => (-50.0), 'y' => 30.0, 'z' => 50.0]);
  $pos = ['cx' => 20.0, 'cy' => 20.0, 'cz' => 0.0, 'r' => 20.0];
  $neg = ['cx' => 1.0, 'cy' => 1.0, 'cz' => (-6.0), 'r' => 20.0];
  $yi = 0;
  while ($yi <= 40) {
  $y = (floatval($yi)) + 0.5;
  $line = '';
  $xi = -20;
  while ($xi <= 60) {
  $x = ((floatval($xi)) - $pos['cx']) / 2.0 + 0.5 + $pos['cx'];
  $hb = hitSphere($pos, $x, $y);
  if (!$hb['hit']) {
  $line = $line . ' ';
  $xi = $xi + 1;
  continue;
}
  $zb1 = $hb['z1'];
  $zb2 = $hb['z2'];
  $hs = hitSphere($neg, $x, $y);
  $hitRes = 1;
  if (!$hs['hit']) {
  $hitRes = 1;
} else {
  if ($hs['z1'] > $zb1) {
  $hitRes = 1;
} else {
  if ($hs['z2'] > $zb2) {
  $hitRes = 0;
} else {
  if ($hs['z2'] > $zb1) {
  $hitRes = 2;
} else {
  $hitRes = 1;
};
};
};
}
  if ($hitRes == 0) {
  $line = $line . ' ';
  $xi = $xi + 1;
  continue;
}
  $vec = null;
  if ($hitRes == 1) {
  $vec = ['x' => $x - $pos['cx'], 'y' => $y - $pos['cy'], 'z' => $zb1 - $pos['cz']];
} else {
  $vec = ['x' => $neg['cx'] - $x, 'y' => $neg['cy'] - $y, 'z' => $neg['cz'] - $hs['z2']];
}
  $vec = normalize($vec);
  $b = powf(dot($light, $vec), 2) + 0.5;
  $intensity = intval(((1.0 - $b) * (floatval(strlen($shades)))));
  if ($intensity < 0) {
  $intensity = 0;
}
  if ($intensity >= strlen($shades)) {
  $intensity = strlen($shades) - 1;
}
  $line = $line . substr($shades, $intensity, $intensity + 1 - $intensity);
  $xi = $xi + 1;
};
  echo rtrim($line), PHP_EOL;
  $yi = $yi + 1;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
