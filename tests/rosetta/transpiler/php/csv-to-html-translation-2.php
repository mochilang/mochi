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
  $c = 'Character,Speech
' . 'The multitude,The messiah! Show us the messiah!
' . 'Brians mother,<angry>Now you listen here! He\'s not the messiah; he\'s a very naughty boy! Now go away!</angry>
' . 'The multitude,Who are you?
' . 'Brians mother,I\'m his mother; that\'s who!
' . 'The multitude,Behold his mother! Behold his mother!';
  $rows = [];
  foreach (explode('
', $c) as $line) {
  $rows = array_merge($rows, [explode(',', $line)]);
}
  $headings = true;
  echo rtrim('<table>'), PHP_EOL;
  if ($headings) {
  if (count($rows) > 0) {
  $th = '';
  foreach ($rows[0] as $h) {
  $th = $th . '<th>' . $h . '</th>';
};
  echo rtrim('   <thead>'), PHP_EOL;
  echo rtrim('      <tr>' . $th . '</tr>'), PHP_EOL;
  echo rtrim('   </thead>'), PHP_EOL;
  echo rtrim('   <tbody>'), PHP_EOL;
  $i = 1;
  while ($i < count($rows)) {
  $cells = '';
  foreach ($rows[$i] as $cell) {
  $cells = $cells . '<td>' . $cell . '</td>';
};
  echo rtrim('      <tr>' . $cells . '</tr>'), PHP_EOL;
  $i = $i + 1;
};
  echo rtrim('   </tbody>'), PHP_EOL;
};
} else {
  foreach ($rows as $row) {
  $cells = '';
  foreach ($row as $cell) {
  $cells = $cells . '<td>' . $cell . '</td>';
};
  echo rtrim('    <tr>' . $cells . '</tr>'), PHP_EOL;
};
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
