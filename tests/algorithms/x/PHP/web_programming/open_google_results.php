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
  function index_of_substr($s, $pat) {
  if (strlen($pat) == 0) {
  return 0;
}
  $i = 0;
  while ($i <= strlen($s) - strlen($pat)) {
  $j = 0;
  while ($j < strlen($pat)) {
  if (substr($s, $i + $j, $i + $j + 1 - ($i + $j)) != substr($pat, $j, $j + 1 - $j)) {
  break;
}
  $j = $j + 1;
};
  if ($j == strlen($pat)) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function first_link($html) {
  $a_idx = index_of_substr($html, '<a');
  if ($a_idx < 0) {
  return '';
}
  $href_pat = 'href="';
  $href_idx_rel = index_of_substr(substr($html, $a_idx), $href_pat);
  if ($href_idx_rel < 0) {
  return '';
}
  $start = $a_idx + $href_idx_rel + strlen($href_pat);
  $end = $start;
  while ($end < strlen($html)) {
  if (substr($html, $end, $end + 1 - $end) == '"') {
  break;
}
  $end = $end + 1;
};
  return substr($html, $start, $end - $start);
};
  $query_words = ['mochi', 'language'];
  $query = '';
  $i = 0;
  while ($i < count($query_words)) {
  if ($i > 0) {
  $query = $query . '%20';
}
  $query = $query . $query_words[$i];
  $i = $i + 1;
}
  $url = 'https://www.google.com/search?q=' . $query . '&num=100';
  echo rtrim('Googling.....'), PHP_EOL;
  $sample_html = '<div><a href="https://example.com">Example</a></div>' . '<div><a href="https://another.com">Another</a></div>';
  $link = first_link($sample_html);
  if (strlen($link) > 0) {
  echo rtrim($link), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
