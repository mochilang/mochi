void main() {
  print(
    (() {
      var _t0 = [1, 2, 3];
      return (_t0.isEmpty ? 0 : _t0.reduce((a, b) => a + b) / _t0.length);
    })(),
  );
}
