<?php
$toks = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if (!$toks || $toks[0] === '') exit;
$idx = 0;
$t = intval($toks[$idx++]);
$cases = [];
for ($tc = 0; $tc < $t; $tc++) {
    $d = intval($toks[$idx++]);
    $e = intval($toks[$idx++]);
    $deptName = [];
    for ($i = 0; $i < $d; $i++) {
        $deptName[intval($toks[$idx++])] = $toks[$idx++];
    }
    $groups = [];
    for ($i = 0; $i < $e; $i++) {
        $idx++;
        $name = $toks[$idx++];
        $salary = intval($toks[$idx++]);
        $deptId = intval($toks[$idx++]);
        $groups[$deptId][] = [$name, $salary];
    }
    $rows = [];
    foreach ($groups as $deptId => $items) {
        $uniq = [];
        foreach ($items as $item) $uniq[$item[1]] = true;
        $salaries = array_keys($uniq);
        rsort($salaries, SORT_NUMERIC);
        $keep = array_flip(array_slice($salaries, 0, 3));
        foreach ($items as $item) if (isset($keep[$item[1]])) $rows[] = [$deptName[$deptId], -$item[1], $item[0], $item[1]];
    }
    usort($rows, fn($a, $b) => $a[0] <=> $b[0] ?: $a[1] <=> $b[1] ?: $a[2] <=> $b[2]);
    $lines = [strval(count($rows))];
    foreach ($rows as $row) $lines[] = "{$row[0]},{$row[2]},{$row[3]}";
    $cases[] = implode("\n", $lines);
}
echo implode("\n\n", $cases);
?>
