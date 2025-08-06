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
$seed = 123456789;
function mochi_rand() {
  global $seed, $graph, $result;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed;
}
function random() {
  global $seed, $graph, $result;
  return (1.0 * mochi_rand()) / 2147483648.0;
}
function sqrtApprox($x) {
  global $seed, $graph, $result;
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function absf($x) {
  global $seed, $graph, $result;
  return ($x < 0.0 ? -$x : $x);
}
function dot($a, $b) {
  global $seed, $graph, $result;
  $s = 0.0;
  $i = 0;
  while ($i < count($a)) {
  $s = $s + $a[$i] * $b[$i];
  $i = $i + 1;
};
  return $s;
}
function vector_scale($v, $s) {
  global $seed, $graph, $result;
  $res = [];
  $i = 0;
  while ($i < count($v)) {
  $res = _append($res, $v[$i] * $s);
  $i = $i + 1;
};
  return $res;
}
function vector_sub($a, $b) {
  global $seed, $graph, $result;
  $res = [];
  $i = 0;
  while ($i < count($a)) {
  $res = _append($res, $a[$i] - $b[$i]);
  $i = $i + 1;
};
  return $res;
}
function vector_add($a, $b) {
  global $seed, $graph, $result;
  $res = [];
  $i = 0;
  while ($i < count($a)) {
  $res = _append($res, $a[$i] + $b[$i]);
  $i = $i + 1;
};
  return $res;
}
function zeros_matrix($r, $c) {
  global $seed, $graph, $result;
  $m = [];
  $i = 0;
  while ($i < $r) {
  $row = [];
  $j = 0;
  while ($j < $c) {
  $row = _append($row, 0.0);
  $j = $j + 1;
};
  $m = _append($m, $row);
  $i = $i + 1;
};
  return $m;
}
function column($m, $idx) {
  global $seed, $graph, $result;
  $col = [];
  $i = 0;
  while ($i < count($m)) {
  $col = _append($col, $m[$i][$idx]);
  $i = $i + 1;
};
  return $col;
}
function validate_adjacency_list($graph) {
  global $seed, $result;
  $i = 0;
  while ($i < count($graph)) {
  $j = 0;
  while ($j < count($graph[$i])) {
  $v = $graph[$i][$j];
  if ($v < 0 || $v >= count($graph)) {
  $panic('Invalid neighbor');
}
  $j = $j + 1;
};
  $i = $i + 1;
};
}
function multiply_matrix_vector($graph, $vector) {
  global $seed;
  $n = count($graph);
  if (count($vector) != $n) {
  $panic('Vector length must match number of nodes');
}
  $result = [];
  $i = 0;
  while ($i < $n) {
  $sum = 0.0;
  $j = 0;
  while ($j < count($graph[$i])) {
  $nb = $graph[$i][$j];
  $sum = $sum + $vector[$nb];
  $j = $j + 1;
};
  $result = _append($result, $sum);
  $i = $i + 1;
};
  return $result;
}
function lanczos_iteration($graph, $k) {
  global $seed, $result;
  $n = count($graph);
  if ($k < 1 || $k > $n) {
  $panic('invalid number of eigenvectors');
}
  $q = zeros_matrix($n, $k);
  $t = zeros_matrix($k, $k);
  $v = [];
  $i = 0;
  while ($i < $n) {
  $v = _append($v, random());
  $i = $i + 1;
};
  $ss = 0.0;
  $i = 0;
  while ($i < $n) {
  $ss = $ss + $v[$i] * $v[$i];
  $i = $i + 1;
};
  $vnorm = sqrtApprox($ss);
  $i = 0;
  while ($i < $n) {
  $q[$i][0] = $v[$i] / $vnorm;
  $i = $i + 1;
};
  $beta = 0.0;
  $j = 0;
  while ($j < $k) {
  $w = multiply_matrix_vector($graph, column($q, $j));
  if ($j > 0) {
  $w = vector_sub($w, vector_scale(column($q, $j - 1), $beta));
}
  $alpha = dot(column($q, $j), $w);
  $w = vector_sub($w, vector_scale(column($q, $j), $alpha));
  $ss2 = 0.0;
  $p = 0;
  while ($p < $n) {
  $ss2 = $ss2 + $w[$p] * $w[$p];
  $p = $p + 1;
};
  $beta = sqrtApprox($ss2);
  $t[$j][$j] = $alpha;
  if ($j < $k - 1) {
  $t[$j][$j + 1] = $beta;
  $t[$j + 1][$j] = $beta;
  if ($beta > 0.0000000001) {
  $wnorm = vector_scale($w, 1.0 / $beta);
  $r = 0;
  while ($r < $n) {
  $q[$r][$j + 1] = $wnorm[$r];
  $r = $r + 1;
};
};
}
  $j = $j + 1;
};
  return ['t' => $t, 'q' => $q];
}
function jacobi_eigen($a_in, $max_iter) {
  global $seed, $graph, $result;
  $n = count($a_in);
  $a = $a_in;
  $v = zeros_matrix($n, $n);
  $i = 0;
  while ($i < $n) {
  $v[$i][$i] = 1.0;
  $i = $i + 1;
};
  $iter = 0;
  while ($iter < $max_iter) {
  $p = 0;
  $q = 1;
  $max = absf($a[$p][$q]);
  $i = 0;
  while ($i < $n) {
  $j = $i + 1;
  while ($j < $n) {
  $val = absf($a[$i][$j]);
  if ($val > $max) {
  $max = $val;
  $p = $i;
  $q = $j;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  if ($max < 0.00000001) {
  break;
}
  $app = $a[$p][$p];
  $aqq = $a[$q][$q];
  $apq = $a[$p][$q];
  $theta = ($aqq - $app) / (2.0 * $apq);
  $t = 1.0 / (absf($theta) + sqrtApprox($theta * $theta + 1.0));
  if ($theta < 0.0) {
  $t = -$t;
}
  $c = 1.0 / sqrtApprox(1.0 + $t * $t);
  $s = $t * $c;
  $tau = $s / (1.0 + $c);
  $a[$p][$p] = $app - $t * $apq;
  $a[$q][$q] = $aqq + $t * $apq;
  $a[$p][$q] = 0.0;
  $a[$q][$p] = 0.0;
  $k = 0;
  while ($k < $n) {
  if ($k != $p && $k != $q) {
  $akp = $a[$k][$p];
  $akq = $a[$k][$q];
  $a[$k][$p] = $akp - $s * ($akq + $tau * $akp);
  $a[$p][$k] = $a[$k][$p];
  $a[$k][$q] = $akq + $s * ($akp - $tau * $akq);
  $a[$q][$k] = $a[$k][$q];
}
  $k = $k + 1;
};
  $k = 0;
  while ($k < $n) {
  $vkp = $v[$k][$p];
  $vkq = $v[$k][$q];
  $v[$k][$p] = $vkp - $s * ($vkq + $tau * $vkp);
  $v[$k][$q] = $vkq + $s * ($vkp - $tau * $vkq);
  $k = $k + 1;
};
  $iter = $iter + 1;
};
  $eigenvalues = [];
  $i = 0;
  while ($i < $n) {
  $eigenvalues = _append($eigenvalues, $a[$i][$i]);
  $i = $i + 1;
};
  return ['values' => $eigenvalues, 'vectors' => $v];
}
function matmul($a, $b) {
  global $seed, $graph, $result;
  $rows = count($a);
  $cols = count($b[0]);
  $inner = count($b);
  $m = zeros_matrix($rows, $cols);
  $i = 0;
  while ($i < $rows) {
  $j = 0;
  while ($j < $cols) {
  $s = 0.0;
  $k = 0;
  while ($k < $inner) {
  $s = $s + $a[$i][$k] * $b[$k][$j];
  $k = $k + 1;
};
  $m[$i][$j] = $s;
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $m;
}
function sort_eigenpairs($vals, $vecs) {
  global $seed, $graph, $result;
  $n = count($vals);
  $values = $vals;
  $vectors = $vecs;
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < $n - 1) {
  if ($values[$j] < $values[$j + 1]) {
  $tmp = $values[$j];
  $values[$j] = $values[$j + 1];
  $values[$j + 1] = $tmp;
  $r = 0;
  while ($r < count($vectors)) {
  $tv = $vectors[$r][$j];
  $vectors[$r][$j] = $vectors[$r][$j + 1];
  $vectors[$r][$j + 1] = $tv;
  $r = $r + 1;
};
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return ['values' => $values, 'vectors' => $vectors];
}
function find_lanczos_eigenvectors($graph, $k) {
  global $seed, $result;
  validate_adjacency_list($graph);
  $res = lanczos_iteration($graph, $k);
  $eig = jacobi_eigen($res['t'], 50);
  $sorted = sort_eigenpairs($eig['values'], $eig['vectors']);
  $final_vectors = matmul($res['q'], $sorted['vectors']);
  return ['values' => $sorted['values'], 'vectors' => $final_vectors];
}
function list_to_string($arr) {
  global $seed, $graph, $result;
  $s = '[';
  $i = 0;
  while ($i < count($arr)) {
  $s = $s . _str($arr[$i]);
  if ($i < count($arr) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  return $s . ']';
}
function matrix_to_string($m) {
  global $seed, $graph, $result;
  $s = '[';
  $i = 0;
  while ($i < count($m)) {
  $s = $s . list_to_string($m[$i]);
  if ($i < count($m) - 1) {
  $s = $s . '; ';
}
  $i = $i + 1;
};
  return $s . ']';
}
$graph = [[1, 2], [0, 2], [0, 1]];
$result = find_lanczos_eigenvectors($graph, 2);
echo rtrim(list_to_string($result['values'])), PHP_EOL;
echo rtrim(matrix_to_string($result['vectors'])), PHP_EOL;
