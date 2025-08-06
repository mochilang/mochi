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
$__start_mem = memory_get_usage();
$__start = _now();
  function update_image_and_anno($all_img_list, $all_annos, $idxs, $output_size, $scale_range, $filter_scale) {
  $height = $output_size[0];
  $width = $output_size[1];
  $output_img = null;
  $r = 0;
  while ($r < $height) {
  $row = null;
  $c = 0;
  while ($c < $width) {
  $row = _append($row, 0);
  $c = $c + 1;
};
  $output_img = _append($output_img, $row);
  $r = $r + 1;
};
  $scale_x = ($scale_range[0] + $scale_range[1]) / 2.0;
  $scale_y = ($scale_range[0] + $scale_range[1]) / 2.0;
  $divid_point_x = (intval(($scale_x * (floatval($width)))));
  $divid_point_y = (intval(($scale_y * (floatval($height)))));
  $new_anno = null;
  $path_list = null;
  $i = 0;
  while ($i < count($idxs)) {
  $index = $idxs[$i];
  $path = $all_img_list[$index];
  $path_list = _append($path_list, $path);
  $img_annos = $all_annos[$index];
  if ($i == 0) {
  $y0 = 0;
  while ($y0 < $divid_point_y) {
  $x0 = 0;
  while ($x0 < $divid_point_x) {
  $output_img[$y0][$x0] = $i + 1;
  $x0 = $x0 + 1;
};
  $y0 = $y0 + 1;
};
  $j0 = 0;
  while ($j0 < count($img_annos)) {
  $bbox = $img_annos[$j0];
  $xmin = $bbox[1] * $scale_x;
  $ymin = $bbox[2] * $scale_y;
  $xmax = $bbox[3] * $scale_x;
  $ymax = $bbox[4] * $scale_y;
  $new_anno = _append($new_anno, [$bbox[0], $xmin, $ymin, $xmax, $ymax]);
  $j0 = $j0 + 1;
};
} else {
  if ($i == 1) {
  $y1 = 0;
  while ($y1 < $divid_point_y) {
  $x1 = $divid_point_x;
  while ($x1 < $width) {
  $output_img[$y1][$x1] = $i + 1;
  $x1 = $x1 + 1;
};
  $y1 = $y1 + 1;
};
  $j1 = 0;
  while ($j1 < count($img_annos)) {
  $bbox1 = $img_annos[$j1];
  $xmin1 = $scale_x + $bbox1[1] * (1.0 - $scale_x);
  $ymin1 = $bbox1[2] * $scale_y;
  $xmax1 = $scale_x + $bbox1[3] * (1.0 - $scale_x);
  $ymax1 = $bbox1[4] * $scale_y;
  $new_anno = _append($new_anno, [$bbox1[0], $xmin1, $ymin1, $xmax1, $ymax1]);
  $j1 = $j1 + 1;
};
} else {
  if ($i == 2) {
  $y2 = $divid_point_y;
  while ($y2 < $height) {
  $x2 = 0;
  while ($x2 < $divid_point_x) {
  $output_img[$y2][$x2] = $i + 1;
  $x2 = $x2 + 1;
};
  $y2 = $y2 + 1;
};
  $j2 = 0;
  while ($j2 < count($img_annos)) {
  $bbox2 = $img_annos[$j2];
  $xmin2 = $bbox2[1] * $scale_x;
  $ymin2 = $scale_y + $bbox2[2] * (1.0 - $scale_y);
  $xmax2 = $bbox2[3] * $scale_x;
  $ymax2 = $scale_y + $bbox2[4] * (1.0 - $scale_y);
  $new_anno = _append($new_anno, [$bbox2[0], $xmin2, $ymin2, $xmax2, $ymax2]);
  $j2 = $j2 + 1;
};
} else {
  $y3 = $divid_point_y;
  while ($y3 < $height) {
  $x3 = $divid_point_x;
  while ($x3 < $width) {
  $output_img[$y3][$x3] = $i + 1;
  $x3 = $x3 + 1;
};
  $y3 = $y3 + 1;
};
  $j3 = 0;
  while ($j3 < count($img_annos)) {
  $bbox3 = $img_annos[$j3];
  $xmin3 = $scale_x + $bbox3[1] * (1.0 - $scale_x);
  $ymin3 = $scale_y + $bbox3[2] * (1.0 - $scale_y);
  $xmax3 = $scale_x + $bbox3[3] * (1.0 - $scale_x);
  $ymax3 = $scale_y + $bbox3[4] * (1.0 - $scale_y);
  $new_anno = _append($new_anno, [$bbox3[0], $xmin3, $ymin3, $xmax3, $ymax3]);
  $j3 = $j3 + 1;
};
};
};
}
  $i = $i + 1;
};
  if ($filter_scale > 0.0) {
  $filtered = null;
  $k = 0;
  while ($k < count($new_anno)) {
  $anno = $new_anno[$k];
  $w = $anno[3] - $anno[1];
  $h = $anno[4] - $anno[2];
  if ($filter_scale < $w && $filter_scale < $h) {
  $filtered = _append($filtered, $anno);
}
  $k = $k + 1;
};
  $new_anno = $filtered;
}
  return ['img' => $output_img, 'annos' => $new_anno, 'path' => $path_list[0]];
};
  function main() {
  $all_img_list = ['img0.jpg', 'img1.jpg', 'img2.jpg', 'img3.jpg'];
  $all_annos = [[[0.0, 0.1, 0.1, 0.4, 0.4]], [[1.0, 0.2, 0.3, 0.5, 0.7]], [[2.0, 0.6, 0.2, 0.9, 0.5]], [[3.0, 0.5, 0.5, 0.8, 0.8]]];
  $idxs = [0, 1, 2, 3];
  $output_size = [100, 100];
  $scale_range = [0.4, 0.6];
  $filter_scale = 0.05;
  $res = update_image_and_anno($all_img_list, $all_annos, $idxs, $output_size, $scale_range, $filter_scale);
  $new_annos = $res['annos'];
  $path = $res['path'];
  echo rtrim('Base image: ' . $path), PHP_EOL;
  echo rtrim('Mosaic annotation count: ' . _str(count($new_annos))), PHP_EOL;
  $i = 0;
  while ($i < count($new_annos)) {
  $a = $new_annos[$i];
  echo rtrim(_str($a[0]) . ' ' . _str($a[1]) . ' ' . _str($a[2]) . ' ' . _str($a[3]) . ' ' . _str($a[4])), PHP_EOL;
  $i = $i + 1;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
