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
  function find_from($s, $pattern, $start) {
  global $sample_html;
  $n = strlen($s);
  $m = strlen($pattern);
  $i = $start;
  while ($i <= $n - $m) {
  if (substr($s, $i, $i + $m - $i) == $pattern) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function download_image($html) {
  global $sample_html;
  $tag = '<meta property="og:image"';
  $idx_tag = find_from($html, $tag, 0);
  if ($idx_tag == (-1)) {
  return 'No meta tag with property \'og:image\' was found.';
}
  $key = 'content="';
  $idx_content = find_from($html, $key, $idx_tag);
  if ($idx_content == (-1)) {
  return 'Image URL not found in meta tag.';
}
  $start = $idx_content + strlen($key);
  $end = $start;
  while ($end < strlen($html) && substr($html, $end, $end + 1 - $end) != '"') {
  $end = $end + 1;
};
  if ($end >= strlen($html)) {
  return 'Image URL not found in meta tag.';
}
  $image_url = substr($html, $start, $end - $start);
  return 'Image URL: ' . $image_url;
};
  $sample_html = '<html><head><meta property="og:image" content="https://example.com/pic.jpg"/></head></html>';
  echo rtrim(download_image($sample_html)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
