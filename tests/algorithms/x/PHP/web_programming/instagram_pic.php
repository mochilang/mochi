<?php
ini_set('memory_limit', '-1');
function find_from($s, $pattern, $start) {
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
}
function download_image($html) {
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
}
$sample_html = '<html><head><meta property="og:image" content="https://example.com/pic.jpg"/></head></html>';
echo rtrim(download_image($sample_html)), PHP_EOL;
