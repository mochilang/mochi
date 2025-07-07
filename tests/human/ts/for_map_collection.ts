const m: Record<string, number> = { a: 1, b: 2 };
for (const k in m) {
  if (Object.prototype.hasOwnProperty.call(m, k)) {
    console.log(k);
  }
}
