// Generated by Mochi compiler v0.10.28 on 2025-07-18T09:34:34Z
List<List<int>> image() {
  return [
    [0, 0, 10000],
    [65535, 65535, 65535],
    [65535, 65535, 65535],
  ];
}

List<int> histogram(List<List<int>> g, int bins) {
  if (bins <= 0) {
    bins = g[0].length;
  }
  List<int> h = [];
  num i = 0;
  while ((i as num) < bins) {
    h = List.from(h)..add(0);
    i = (i as num) + 1;
  }
  num y = 0;
  while ((y as num) < g.length) {
    var row = g[y];
    num x = 0;
    while ((x as num) < row.length) {
      var p = row[x];
      var idx = int.parse(((((p as num) * (bins - 1)) as num) / 65535));
      h[idx] = (h[idx] as num) + 1;
      x = (x as num) + 1;
    }
    y = (y as num) + 1;
  }
  return h;
}

int medianThreshold(List<int> h) {
  num lb = 0;
  var ub = h.length - 1;
  num lSum = 0;
  num uSum = 0;
  while ((lb as num) <= (ub as num)) {
    if (((lSum as num) + h[lb] as num) < ((uSum as num) + h[ub] as num)) {
      lSum = (lSum as num) + h[lb];
      lb = (lb as num) + 1;
    }
    else {
      uSum = (uSum as num) + h[ub];
      ub = (ub as num) - 1;
    }
  }
  return int.parse(((((ub as num) * 65535) as num) / h.length));
}

List<List<int>> threshold(List<List<int>> g, int t) {
  List<List<int>> out = [];
  num y = 0;
  while ((y as num) < g.length) {
    var row = g[y];
    List<int> newRow = [];
    num x = 0;
    while ((x as num) < row.length) {
      if ((row[x] as num) < t) {
        newRow = List.from(newRow)..add(0);
      }
      else {
        newRow = List.from(newRow)..add(65535);
      }
      x = (x as num) + 1;
    }
    out = List.from(out)..add(newRow);
    y = (y as num) + 1;
  }
  return out;
}

void printImage(List<List<int>> g) {
  num y = 0;
  while ((y as num) < g.length) {
    var row = g[y];
    var line = '';
    num x = 0;
    while ((x as num) < row.length) {
      if (row[x] == 0) {
        line = line + '0';
      }
      else {
        line = line + '1';
      }
      x = (x as num) + 1;
    }
    print(line);
    y = (y as num) + 1;
  }
}

void _main() {
  var img = image();
  var h = histogram(img, 0);
  print('Histogram: ' + h.toString());
  var t = medianThreshold(h);
  print('Threshold: ' + t.toString());
  var bw = threshold(img, t);
  printImage(bw);
}

void main() {
  _main();
}
