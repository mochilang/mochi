<?php
ini_set('memory_limit', '-1');
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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function find_index($s, $pat, $start) {
  $i = $start;
  while ($i <= strlen($s) - strlen($pat)) {
  $j = 0;
  $ok = true;
  while ($j < strlen($pat)) {
  if (substr($s, $i + $j, $i + $j + 1 - ($i + $j)) != substr($pat, $j, $j + 1 - $j)) {
  $ok = false;
  break;
}
  $j = $j + 1;
};
  if ($ok) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function slice_between($s, $start_pat, $end_pat, $from) {
  $a = find_index($s, $start_pat, $from);
  if ($a < 0) {
  return '';
}
  $b = $a + strlen($start_pat);
  $c = find_index($s, $end_pat, $b);
  if ($c < 0) {
  return '';
}
  return substr($s, $b, $c - $b);
}
function char_to_digit($c) {
  if ($c == '0') {
  return 0;
}
  if ($c == '1') {
  return 1;
}
  if ($c == '2') {
  return 2;
}
  if ($c == '3') {
  return 3;
}
  if ($c == '4') {
  return 4;
}
  if ($c == '5') {
  return 5;
}
  if ($c == '6') {
  return 6;
}
  if ($c == '7') {
  return 7;
}
  if ($c == '8') {
  return 8;
}
  return 9;
}
function parse_int($txt) {
  $n = 0;
  $i = 0;
  while ($i < strlen($txt)) {
  $c = substr($txt, $i, $i + 1 - $i);
  if ($c >= '0' && $c <= '9') {
  $n = $n * 10 + char_to_digit($c);
}
  $i = $i + 1;
};
  return $n;
}
function parse_product($block) {
  $href = slice_between($block, 'href="', '"', 0);
  $link = 'https://www.amazon.in' . $href;
  $title = slice_between($block, '>', '</a>', find_index($block, '<a', 0));
  $price = slice_between($block, '<span class="a-offscreen">', '</span>', 0);
  $rating = slice_between($block, '<span class="a-icon-alt">', '</span>', 0);
  if (strlen($rating) == 0) {
  $rating = 'Not available';
}
  $mrp = slice_between($block, '<span class="a-price a-text-price">', '</span>', 0);
  $disc = 0.0;
  if (strlen($mrp) > 0 && strlen($price) > 0) {
  $p = parse_int($price);
  $m = parse_int($mrp);
  if ($m > 0) {
  $disc = floatval((($m - $p) * 100)) / (floatval($m));
};
} else {
  $mrp = '';
  $disc = 0.0;
}
  return ['title' => $title, 'link' => $link, 'price' => $price, 'rating' => $rating, 'mrp' => $mrp, 'discount' => $disc];
}
function get_amazon_product_data($product) {
  $html = '<div class="s-result-item" data-component-type="s-search-result"><h2><a href="/sample_product">Sample Product</a></h2><span class="a-offscreen">₹900</span><span class="a-icon-alt">4.3 out of 5 stars</span><span class="a-price a-text-price">₹1000</span></div><div class="s-result-item" data-component-type="s-search-result"><h2><a href="/item2">Another Product</a></h2><span class="a-offscreen">₹500</span><span class="a-icon-alt">3.8 out of 5 stars</span><span class="a-price a-text-price">₹800</span></div>';
  $out = [];
  $start = 0;
  while (true) {
  $div_start = find_index($html, '<div class="s-result-item"', $start);
  if ($div_start < 0) {
  break;
}
  $div_end = find_index($html, '</div>', $div_start);
  if ($div_end < 0) {
  break;
}
  $block = substr($html, $div_start, $div_end - $div_start);
  $out = _append($out, parse_product($block));
  $start = $div_end + strlen('</div>');
};
  return $out;
}
function main() {
  $products = get_amazon_product_data('laptop');
  $i = 0;
  while ($i < count($products)) {
  $p = $products[$i];
  echo rtrim($p['title'] . ' | ' . $p['link'] . ' | ' . $p['price'] . ' | ' . $p['rating'] . ' | ' . $p['mrp'] . ' | ' . _str($p['discount'])), PHP_EOL;
  $i = $i + 1;
};
}
main();
