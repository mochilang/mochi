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
function gzipWriter($w) {
  return $w;
}
function tarWriter($w) {
  return $w;
}
function tarWriteHeader($w, $hdr) {
}
function tarWrite($w, $data) {
}
function main() {
  $filename = 'TAPE.FILE';
  $data = '';
  $outfile = '';
  $gzipFlag = false;
  $w = null;
  if ($outfile != '') {
  $w = null;
}
  if ($gzipFlag) {
  $w = gzipWriter($w);
}
  $w = tarWriter($w);
  $hdr = ['Name' => $filename, 'Mode' => 432, 'Size' => strlen($data), 'ModTime' => _now(), 'Typeflag' => 0, 'Uname' => 'guest', 'Gname' => 'guest'];
  tarWriteHeader($w, $hdr);
  tarWrite($w, $data);
}
main();
