<?php
function writeTwo() {
  global $appendOneMore, $main;
  return ["jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash", "jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jsmith:/bin/bash"];
}
function appendOneMore(&$lines) {
  global $writeTwo, $main;
  return array_merge($lines, ["xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash"]);
}
function main() {
  global $writeTwo, $appendOneMore;
  $lines = writeTwo();
  $lines = appendOneMore($lines);
  if (count($lines) >= 3 && $lines[2] == "xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash") {
  echo "append okay", PHP_EOL;
} else {
  echo "it didn't work", PHP_EOL;
}
}
main();
