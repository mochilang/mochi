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
  function showState($w, $label) {
  echo rtrim($label . ': pos=(' . _str($w['x']) . ',' . _str($w['y']) . ') size=(' . _str($w['w']) . 'x' . _str($w['h']) . ') max=' . _str($w['maximized']) . ' icon=' . _str($w['iconified']) . ' visible=' . _str($w['visible'])), PHP_EOL;
};
  function maximize(&$w) {
  $w['maximized'] = true;
  $w['w'] = 800;
  $w['h'] = 600;
  return $w;
};
  function unmaximize(&$w) {
  $w['maximized'] = false;
  $w['w'] = 640;
  $w['h'] = 480;
  return $w;
};
  function iconify(&$w) {
  $w['iconified'] = true;
  $w['visible'] = false;
  return $w;
};
  function deiconify(&$w) {
  $w['iconified'] = false;
  $w['visible'] = true;
  return $w;
};
  function hide(&$w) {
  $w['visible'] = false;
  return $w;
};
  function showWindow(&$w) {
  $w['visible'] = true;
  return $w;
};
  function move(&$w) {
  if ($w['shifted']) {
  $w['x'] = $w['x'] - 10;
  $w['y'] = $w['y'] - 10;
} else {
  $w['x'] = $w['x'] + 10;
  $w['y'] = $w['y'] + 10;
}
  $w['shifted'] = !$w['shifted'];
  return $w;
};
  function main() {
  $win = ['x' => 100, 'y' => 100, 'w' => 640, 'h' => 480, 'maximized' => false, 'iconified' => false, 'visible' => true, 'shifted' => false];
  showState($win, 'Start');
  $win = maximize($win);
  showState($win, 'Maximize');
  $win = unmaximize($win);
  showState($win, 'Unmaximize');
  $win = iconify($win);
  showState($win, 'Iconify');
  $win = deiconify($win);
  showState($win, 'Deiconify');
  $win = hide($win);
  showState($win, 'Hide');
  $win = showWindow($win);
  showState($win, 'Show');
  $win = move($win);
  showState($win, 'Move');
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
