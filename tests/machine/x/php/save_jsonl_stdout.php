<?php
$people = [["name" => "Alice", "age" => 30], ["name" => "Bob", "age" => 25]];
_save($people, "-", ["format" => "jsonl"]);
function _save($rows, $path = null, $opts = []) {
    $fmt = $opts['format'] ?? 'csv';
    if ($fmt === 'jsonl') {
        $out = ($path === null || $path === '' || $path === '-') ? STDOUT : fopen($path, 'w');
        foreach ($rows as $row) {
            if (is_object($row)) $row = (array)$row;
            fwrite($out, json_encode($row) . PHP_EOL);
        }
        if ($out !== STDOUT) fclose($out);
    }
}?>
