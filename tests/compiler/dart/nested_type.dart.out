Map<String, Function> _structParsers = {};

int sumPoint() {
  class Point {
    int x;
    int y;
    Point({required this.x, required this.y});
    factory Point.fromJson(Map<String,dynamic> m) {
      return Point(x: m['x'] as int, y: m['y'] as int);
    }
  }
  
  dynamic p = Point(x: 2, y: 3);
  return (p.x + p.y);
}

void main() {
  _structParsers['Point'] = (m) => Point.fromJson(m);
  
  print(sumPoint());
}
