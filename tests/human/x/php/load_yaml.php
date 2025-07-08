<?php
$file = __DIR__ . '/../../../interpreter/valid/people.yaml';
$lines = file($file, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
$people = [];
$current = [];
foreach ($lines as $line) {
    $line = trim($line);
    if (str_starts_with($line, '-')) {
        if ($current) $people[] = $current;
        $current = [];
        $line = trim(substr($line,1));
        if ($line !== '') {
            [$k,$v] = array_map('trim', explode(':', $line, 2));
            $current[$k] = is_numeric($v) ? (int)$v : $v;
        }
    } else {
        [$k,$v] = array_map('trim', explode(':', $line, 2));
        $current[$k] = is_numeric($v) ? (int)$v : $v;
    }
}
if ($current) $people[] = $current;
$adults = array_filter($people, fn($p) => $p['age'] >= 18);
foreach ($adults as $a) {
    var_dump($a['name'], $a['email']);
}
?>
