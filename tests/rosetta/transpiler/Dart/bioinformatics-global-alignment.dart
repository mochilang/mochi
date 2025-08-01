// Generated by Mochi transpiler
import 'dart:convert';
import 'dart:io';

int _nowSeed = 0;
bool _nowSeeded = false;
void _initNow() {
  var s = Platform.environment['MOCHI_NOW_SEED'];
  if (s != null && s.isNotEmpty) {
    var v = int.tryParse(s);
    if (v != null) {
      _nowSeed = v;
      _nowSeeded = true;
    }
  }
}
int _now() {
  if (_nowSeeded) {
    _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647;
    return _nowSeed;
  }
  return DateTime.now().microsecondsSinceEpoch;
}

String _substr(String s, int start, int end) {
  var n = s.length;
  if (start < 0) start += n;
  if (end < 0) end += n;
  if (start < 0) start = 0;
  if (start > n) start = n;
  if (end < 0) end = 0;
  if (end > n) end = n;
  if (start > end) start = end;
  return s.substring(start, end);
}

String padLeft(String s, int w) {
  String res = "";
  int n = w - s.length;
  while (n > 0) {
    res = res + " ";
    n = n - 1;
  }
  return res + s;
}

int indexOfFrom(String s, String ch, int start) {
  int i = start;
  while (i < s.length) {
    if (_substr(s, i, i + 1) == ch) {
    return i;
  }
    i = i + 1;
  }
  return -1;
}

bool containsStr(String s, String sub) {
  int i = 0;
  int sl = s.length;
  int subl = sub.length;
  while (i <= sl - subl) {
    if (_substr(s, i, i + subl) == sub) {
    return true;
  }
    i = i + 1;
  }
  return false;
}

List<String> distinct(List<String> slist) {
  List<String> res = <String>[];
  for (String s in slist) {
    bool found = false;
    for (String r in res) {
    if (r == s) {
    found = true;
    break;
  }
  }
    if (!found) {
    res = [...res, s];
  }
  }
  return res;
}

List<List<String>> permutations(List<String> xs) {
  if (xs.length <= 1) {
    return [xs];
  }
  List<List<String>> res = <List<String>>[];
  int i = 0;
  while (i < xs.length) {
    List<String> rest = <String>[];
    int j = 0;
    while (j < xs.length) {
    if (j != i) {
    rest = [...rest, xs[j]];
  }
    j = j + 1;
  }
    List<List<String>> subs = permutations(rest);
    for (List<String> p in subs) {
    List<String> perm = [xs[i]];
    int k = 0;
    while (k < p.length) {
    perm = [...perm, p[k]];
    k = k + 1;
  }
    res = ([...res, perm] as List).map((e) => List<String>.from(e)).toList();
  }
    i = i + 1;
  }
  return res;
}

int headTailOverlap(String s1, String s2) {
  int start = 0;
  while (true) {
    int ix = indexOfFrom(s1, _substr(s2, 0, 1), start);
    if (ix == 0 - 1) {
    return 0;
  }
    start = ix;
    int sublen = s1.length - start;
    if (sublen > s2.length) {
    sublen = s2.length;
  }
    if (_substr(s2, 0, sublen) == _substr(s1, start, start + sublen)) {
    return sublen;
  }
    start = start + 1;
  }
}

List<String> deduplicate(List<String> slist) {
  List<String> arr = distinct(slist);
  List<String> filtered = <String>[];
  int i = 0;
  while (i < arr.length) {
    String s1 = arr[i];
    bool within = false;
    int j = 0;
    while (j < arr.length) {
    if (j != i && containsStr(arr[j], s1)) {
    within = true;
    break;
  }
    j = j + 1;
  }
    if (!within) {
    filtered = [...filtered, s1];
  }
    i = i + 1;
  }
  return filtered;
}

String joinAll(List<String> ss) {
  String out = "";
  for (String s in ss) {
    out = out + s;
  }
  return out;
}

String shortestCommonSuperstring(List<String> slist) {
  List<String> ss = deduplicate(slist);
  String shortest = joinAll(ss);
  List<List<String>> perms = permutations(ss);
  int idx = 0;
  while (idx < perms.length) {
    List<String> perm = perms[idx];
    String sup = perm[0];
    int i = 0;
    while (i < ss.length - 1) {
    int ov = headTailOverlap(perm[i], perm[i + 1]);
    sup = sup + _substr(perm[i + 1], ov, perm[i + 1].length);
    i = i + 1;
  }
    if (sup.length < shortest.length) {
    shortest = sup;
  }
    idx = idx + 1;
  }
  return shortest;
}

void printCounts(String seq) {
  int a = 0;
  int c = 0;
  int g = 0;
  int t = 0;
  int i = 0;
  while (i < seq.length) {
    String ch = _substr(seq, i, i + 1);
    if (ch == "A") {
    a = a + 1;
  } else {
    if (ch == "C") {
    c = c + 1;
  } else {
    if (ch == "G") {
    g = g + 1;
  } else {
    if (ch == "T") {
    t = t + 1;
  };
  };
  };
  }
    i = i + 1;
  }
  int total = seq.length;
  print("\nNucleotide counts for " + seq + ":\n");
  print(padLeft("A", 10) + padLeft((a).toString(), 12));
  print(padLeft("C", 10) + padLeft((c).toString(), 12));
  print(padLeft("G", 10) + padLeft((g).toString(), 12));
  print(padLeft("T", 10) + padLeft((t).toString(), 12));
  print(padLeft("Other", 10) + padLeft((total - (a + c + g + t)).toString(), 12));
  print("  ____________________");
  print(padLeft("Total length", 14) + padLeft((total).toString(), 8));
}

void _main() {
  List<List<String>> tests = [["TA", "AAG", "TA", "GAA", "TA"], ["CATTAGGG", "ATTAG", "GGG", "TA"], ["AAGAUGGA", "GGAGCGCAUC", "AUCGCAAUAAGGA"], ["ATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTAT", "GGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGT", "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA", "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "AACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT", "GCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTC", "CGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATTCTGCTTATAACACTATGTTCT", "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATGCTCGTGC", "GATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATT", "TTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA", "TCTCTTAAACTCCTGCTAAATGCTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGA"]];
  for (List<String> seqs in tests) {
    String scs = shortestCommonSuperstring(seqs);
    printCounts(scs);
  }
}

void _start() {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  _initNow();
  {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  _main();
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "_start"}));
}

void main() => _start();
