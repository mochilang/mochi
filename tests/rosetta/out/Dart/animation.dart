// Generated by Mochi compiler v0.10.28 on 2025-07-18T09:33:50Z
var msg = 'Hello World! ';

num shift = 0;

var inc = 1;

num clicks = 0;

num frames = 0;

void main() {
  while (clicks < 5) {
    var line = '';
    num i = 0;
    while ((i as num) < msg.length) {
      var idx = ((shift + (i as num)) as num) % msg.length;
      line = line + msg.substring(idx, (idx as num) + 1);
      i = (i as num) + 1;
    }
    print(line);
    shift = (shift + inc) % msg.length;
    frames = frames + 1;
    if (frames % msg.length == 0) {
      inc = msg.length - inc;
      clicks = clicks + 1;
    }
  }
}
