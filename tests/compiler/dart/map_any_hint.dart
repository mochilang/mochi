Map<String, dynamic> Leaf() {
  return {"__name": "Leaf"};
}

Map<String, dynamic> Node(Map<String, dynamic> left, int value, Map<String, dynamic> right) {
  return {"__name": "Node", "left": left, "value": value, "right": right};
}

Map<String, dynamic> tree = Node(Leaf(), 1, Leaf());

void main() {
  print(((tree["left"] as Map<String, dynamic>))["__name"]);
}
