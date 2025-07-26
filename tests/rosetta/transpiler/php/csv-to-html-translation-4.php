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
  echo rtrim('<table>'), PHP_EOL;
  echo rtrim('    <tr><td>Character</td><td>Speech</td></tr>'), PHP_EOL;
  echo rtrim('    <tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>'), PHP_EOL;
  echo rtrim('    <tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He&#39;s not the messiah; he&#39;s a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>'), PHP_EOL;
  echo rtrim('    <tr><td>The multitude</td><td>Who are you?</td></tr>'), PHP_EOL;
  echo rtrim('    <tr><td>Brians mother</td><td>I&#39;m his mother; that&#39;s who!</td></tr>'), PHP_EOL;
  echo rtrim('    <tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>'), PHP_EOL;
  echo rtrim('</table>'), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
