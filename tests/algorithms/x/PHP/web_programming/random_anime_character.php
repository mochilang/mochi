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
  $seed = 123456789;
  function mochi_rand() {
  global $seed;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed;
};
  function mochi_random_int($a, $b) {
  return $a + (fmod(mochi_rand(), ($b - $a)));
};
  $characters = [['title' => 'Naruto Uzumaki', 'description' => 'A spirited ninja of the Hidden Leaf Village.', 'image_file' => 'naruto.png'], ['title' => 'Sailor Moon', 'description' => 'A magical girl who fights for love and justice.', 'image_file' => 'sailor_moon.png'], ['title' => 'Spike Spiegel', 'description' => 'A bounty hunter with a laid-back attitude.', 'image_file' => 'spike_spiegel.png']];
  function save_image($_name) {
};
  function random_anime_character() {
  global $characters;
  $idx = mochi_random_int(0, count($characters));
  $ch = $characters[$idx];
  save_image($ch['image_file']);
  return $ch;
};
  $c = random_anime_character();
  echo rtrim(json_encode($c['title'], 1344)), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  echo rtrim(json_encode($c['description'], 1344)), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  echo rtrim('Image saved : ' . $c['image_file']), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
