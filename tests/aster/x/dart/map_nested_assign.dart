// Generated by Mochi transpiler
class Data {
  final int inner;
  const Data({required this.inner});
}
class S1 {
  final Data outer;
  const S1({required this.outer});
}
void main() {
  S1 data = S1(outer: Data(inner: 1));
   = 2;
  print(data["outer"]["inner"]);
}
