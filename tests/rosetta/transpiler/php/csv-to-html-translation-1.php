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
  $c = 'Character,Speech\n' . 'The multitude,The messiah! Show us the messiah!\n' . 'Brians mother,<angry>Now you listen here! He\'s not the messiah; he\'s a very naughty boy! Now go away!</angry>\n' . 'The multitude,Who are you?\n' . 'Brians mother,I\'m his mother; that\'s who!\n' . 'The multitude,Behold his mother! Behold his mother!';
  $rows = [];
  foreach (explode('\n', $c) as $line) {
  $rows = array_merge($rows, [explode(',', $line)]);
}
  echo rtrim('<table>'), PHP_EOL;
  foreach ($rows as $row) {
  $cells = '';
  foreach ($row as $cell) {
  $cells = $cells . '<td>' . $cell . '</td>';
};
  echo rtrim('    <tr>' . $cells . '</tr>'), PHP_EOL;
}
  echo rtrim('</table>'), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
