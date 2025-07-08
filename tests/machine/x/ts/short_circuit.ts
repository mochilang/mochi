function boom(a, b) {
  console.log("boom");
  return true;
}
console.log((false && boom(1, 2)));
console.log((true || boom(1, 2)));
