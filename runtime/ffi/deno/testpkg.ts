/** Pi is the mathematical constant pi. */
export const PI = 3.14;

/** Answer is the answer to life, the universe, and everything. */
export let answer = 42;

/** Point represents a point in 2D space. */
export interface Point {
  /** X coordinate */
  x: number;
  /** Y coordinate */
  y: number;
}

/**
 * add returns the sum of a and b.
 */
export function add(a: number, b: number): number {
  return a + b;
}

/**
 * fail always throws an error.
 */
export function fail(): never {
  throw new Error("boom");
}
