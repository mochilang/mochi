// Generated by Mochi transpiler
class Combo {
  final int n;
  final String l;
  final bool b;
  const Combo({required this.n, required this.l, required this.b});
}
void main() {
  final List<int> nums = [1, 2];
  final List<String> letters = ["A", "B"];
  final List<bool> bools = [true, false];
  final List<Combo> combos = [for (var n in nums) for (var l in letters) for (var b in bools) Combo(n: n, l: l, b: b)];
  print("--- Cross Join of three lists ---");
  for (var c in combos) {
    print([c.n, c.l, c.b].join(" "));
  }
}
