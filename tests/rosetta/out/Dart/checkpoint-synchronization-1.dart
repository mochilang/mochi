// Generated by Mochi compiler v0.10.28 on 2025-07-18T09:35:13Z
var partList = [
  'A',
  'B',
  'C',
  'D',
];

var nAssemblies = 3;

void main() {
  for (var cycle = 1; cycle < (nAssemblies + 1); cycle++) {
    print('begin assembly cycle ' + cycle.toString());
    for (var p in partList) {
      print(p + ' worker begins part');
    }
    for (var p in partList) {
      print(p + ' worker completes part');
    }
    print('assemble.  cycle ' + cycle.toString() + ' complete');
  }
}
