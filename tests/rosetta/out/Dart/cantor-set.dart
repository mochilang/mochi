// Generated by Mochi compiler v0.10.28 on 2025-07-18T09:34:58Z
var width = 81;

var height = 5;

List<String> lines = [];

String setChar(String s, int idx, String ch) {
  return s.toString().substring(0, idx) + ch + s.toString().substring(idx + 1, s.length);
}

List<Map<String, int>> stack = [
  {'start': 0, 'len': width, 'index': 1},
];

void main() {
  for (var i = 0; i < height; i++) {
    var row = '';
    num j = 0;
    while ((j as num) < width) {
      row = row + '*';
      j = (j as num) + 1;
    }
    lines = List.from(lines)..add(row);
  }
  while (stack.length > 0) {
    var frame = stack[stack.length - 1];
    stack = stack.sublist(0, stack.length - 1);
    var start = frame['start'];
    var lenSeg = frame['len'];
    var index = frame['index'];
    var seg = int.parse(((lenSeg as num) / 3));
    if (seg == 0) {
      continue;
    }
    var i = index;
    while ((i as num) < height) {
      var j = (start as num) + (seg as num);
      while ((j as num) < ((start as num) + (2 * (seg as num) as num) as num)) {
        lines[i] = setChar(lines[i], j, ' ');
        j = (j as num) + 1;
      }
      i = (i as num) + 1;
    }
    stack = List.from(stack)..add({
      'start': start,
      'len': seg,
      'index': (index as num) + 1,
    });
    stack = List.from(stack)..add({
      'start': (start as num) + ((seg as num) * 2 as num),
      'len': seg,
      'index': (index as num) + 1,
    });
  }
  for (var line in lines) {
    print(line);
  }
}
