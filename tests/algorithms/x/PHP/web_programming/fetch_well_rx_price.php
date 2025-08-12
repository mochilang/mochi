<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$SAMPLE_HTML = '<div class="grid-x pharmCard"><p class="list-title">Pharmacy A</p><span class="price price-large">$10.00</span></div><div class="grid-x pharmCard"><p class="list-title">Pharmacy B</p><span class="price price-large">$12.50</span></div>';
function find_substring($s, $sub, $from) {
  $i = $from;
  while ($i <= strlen($s) - strlen($sub)) {
  $j = 0;
  while ($j < strlen($sub) && substr($s, $i + $j, $i + $j + 1 - ($i + $j)) == substr($sub, $j, $j + 1 - $j)) {
  $j = $j + 1;
};
  if ($j == strlen($sub)) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function fetch_pharmacy_and_price_list($drug_name, $zip_code) {
  global $SAMPLE_HTML;
  if ($drug_name == '' || $zip_code == '') {
  return null;
}
  $results = [];
  $pos = 0;
  $block_tag = '<div class="grid-x pharmCard">';
  $name_tag = '<p class="list-title">';
  $price_tag = '<span class="price price-large">';
  while (true) {
  $div_idx = find_substring($SAMPLE_HTML, $block_tag, $pos);
  if ($div_idx < 0) {
  break;
}
  $name_start = find_substring($SAMPLE_HTML, $name_tag, $div_idx);
  if ($name_start < 0) {
  break;
}
  $name_start = $name_start + strlen($name_tag);
  $name_end = find_substring($SAMPLE_HTML, '</p>', $name_start);
  if ($name_end < 0) {
  break;
}
  $name = substr($SAMPLE_HTML, $name_start, $name_end - $name_start);
  $price_start = find_substring($SAMPLE_HTML, $price_tag, $name_end);
  if ($price_start < 0) {
  break;
}
  $price_start = $price_start + strlen($price_tag);
  $price_end = find_substring($SAMPLE_HTML, '</span>', $price_start);
  if ($price_end < 0) {
  break;
}
  $price = substr($SAMPLE_HTML, $price_start, $price_end - $price_start);
  $results = _append($results, ['pharmacy_name' => $name, $price => $price]);
  $pos = $price_end;
};
  return $results;
}
$pharmacy_price_list = fetch_pharmacy_and_price_list('aspirin', '30303');
if ($pharmacy_price_list != null) {
  $i = 0;
  while ($i < count($pharmacy_price_list)) {
  $entry = $pharmacy_price_list[$i];
  echo rtrim('Pharmacy: ' . $entry['pharmacy_name'] . ' Price: ' . $entry['price']), PHP_EOL;
  $i = $i + 1;
};
} else {
  echo rtrim('No results found'), PHP_EOL;
}
