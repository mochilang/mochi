// Generated by Mochi v0.10.40 on 2025-07-25 17:40:53 GMT+7

export interface Base { name: string; price: number; color: string }
export interface Update { price: number; color: string; year: number }
function merge(base: Record<string, any>, update: Record<string, any>): Record<string, any> {
  let result: Record<string, any> = {};
  for (const k in base) {
    result[k] = base[k];
  }
  for (const k in update) {
    result[k] = update[k];
  }
  return result;
}
function main() {
  const base: Base = {"name": "Rocket Skates", "price": 12.75, "color": "yellow"};
  const update: Update = {"price": 15.25, "color": "red", "year": 1974};
  const result = merge(base, update);
  console.log(result);
}
main();
