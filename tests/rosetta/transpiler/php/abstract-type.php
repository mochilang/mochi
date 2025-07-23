<?php
function beastKind($b) {
  global $beastName, $beastCry, $bprint, $main;
  return (function($__v) {
  if ($__v['__tag'] === "Dog") {
    $k = $__v["kind"];
    return $k;
  } elseif ($__v['__tag'] === "Cat") {
    $k = $__v["kind"];
    return $k;
  }
})($b);
}
function beastName($b) {
  global $beastKind, $beastCry, $bprint, $main;
  return (function($__v) {
  if ($__v['__tag'] === "Dog") {
    $n = $__v["name"];
    return $n;
  } elseif ($__v['__tag'] === "Cat") {
    $n = $__v["name"];
    return $n;
  }
})($b);
}
function beastCry($b) {
  global $beastKind, $beastName, $bprint, $main;
  return (function($__v) {
  if ($__v['__tag'] === "Dog") {
    return "Woof";
  } elseif ($__v['__tag'] === "Cat") {
    return "Meow";
  }
})($b);
}
function bprint($b) {
  global $beastKind, $beastName, $beastCry, $main;
  echo beastName($b) . ", who's a " . beastKind($b) . ", cries: \"" . beastCry($b) . "\".", PHP_EOL;
}
function main() {
  global $beastKind, $beastName, $beastCry, $bprint;
  $d = ["__tag" => "Dog", "kind" => "labrador", "name" => "Max"];
  $c = ["__tag" => "Cat", "kind" => "siamese", "name" => "Sammy"];
  bprint($d);
  bprint($c);
}
main();
