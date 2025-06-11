export const answer = 42;
export let counter = 0;
export function add(a: number, b: number): number { return a + b; }
export const square = (n: number): number => n * n;
export class Point { constructor(public x: number, public y: number) {} }
export enum Color { Red, Blue }
export type Pair = [number, number];
export interface Named { name: string }
