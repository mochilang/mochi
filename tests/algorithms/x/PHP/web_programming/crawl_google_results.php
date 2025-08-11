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
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
function index_of_from($s, $sub, $start) {
  $i = $start;
  $max = strlen($s) - strlen($sub);
  while ($i <= $max) {
  if (substr($s, $i, _iadd($i, strlen($sub)) - $i) == $sub) {
  return $i;
}
  $i = _iadd($i, 1);
};
  return -1;
}
function extract_links($html) {
  $res = [];
  $i = 0;
  while (true) {
  $tag_start = index_of_from($html, '<a class="eZt8xd"', $i);
  if ($tag_start == (-1)) {
  break;
}
  $href_start = index_of_from($html, 'href="', $tag_start);
  if ($href_start == (-1)) {
  break;
}
  $href_start = _iadd($href_start, strlen('href="'));
  $href_end = index_of_from($html, '"', $href_start);
  if ($href_end == (-1)) {
  break;
}
  $href = substr($html, $href_start, $href_end - $href_start);
  $text_start = _iadd(index_of_from($html, '>', $href_end), 1);
  $text_end = index_of_from($html, '</a>', $text_start);
  if ($text_end == (-1)) {
  break;
}
  $text = substr($html, $text_start, $text_end - $text_start);
  $link = ['href' => $href, 'text' => $text];
  $res = _append($res, $link);
  $i = _iadd($text_end, strlen('</a>'));
};
  return $res;
}
function main() {
  $html = '<div><a class="eZt8xd" href="/url?q=http://example1.com">Example1</a>' . '<a class="eZt8xd" href="/maps">Maps</a>' . '<a class="eZt8xd" href="/url?q=http://example2.com">Example2</a></div>';
  $links = extract_links($html);
  echo rtrim(_str(count($links))), PHP_EOL;
  $i = 0;
  while ($i < count($links) && $i < 5) {
  $link = $links[$i];
  $href = $link['href'];
  $text = $link['text'];
  if ($text == 'Maps') {
  echo rtrim($href), PHP_EOL;
} else {
  echo rtrim('https://google.com' . $href), PHP_EOL;
}
  $i = _iadd($i, 1);
};
}
main();
