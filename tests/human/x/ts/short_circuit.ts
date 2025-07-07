function boom(a: number, b: number): boolean {
  console.log("boom");
  return true;
}

console.log(false && boom(1,2));
console.log(true || boom(1,2));
