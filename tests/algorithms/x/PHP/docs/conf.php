<?php
ini_set('memory_limit', '-1');
function parse_project_name($toml) {
  global $pyproject, $project;
  $i = 0;
  $name = '';
  $n = strlen($toml);
  while ($i + 4 < $n) {
  if (substr($toml, $i, $i + 1 - $i) == 'n' && substr($toml, $i + 1, $i + 1 + 1 - ($i + 1)) == 'a' && substr($toml, $i + 2, $i + 2 + 1 - ($i + 2)) == 'm' && substr($toml, $i + 3, $i + 3 + 1 - ($i + 3)) == 'e') {
  $i = $i + 4;
  while ($i < $n && substr($toml, $i, $i + 1 - $i) != '"') {
  $i = $i + 1;
};
  $i = $i + 1;
  while ($i < $n && substr($toml, $i, $i + 1 - $i) != '"') {
  $name = $name . substr($toml, $i, $i + 1 - $i);
  $i = $i + 1;
};
  return $name;
}
  $i = $i + 1;
};
  return $name;
}
$pyproject = '[project]
name = "thealgorithms-python"';
$project = parse_project_name($pyproject);
echo rtrim($project), PHP_EOL;
