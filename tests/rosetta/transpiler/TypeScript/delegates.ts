// Generated by Mochi v0.10.41 on 2025-07-26 17:26:41 GMT+7

export type Fn = any;
export interface Delegator { delegate: Record<string, any> }
function operation(d: Delegator): string {
  if (("thing" in d.delegate)) {
    return d.delegate.thing();
  }
  return "default implementation";
}
function newDelegate(): Record<string, any> {
  let m: Record<string, any> = {};
  m.thing = () => {
return "delegate implementation";
};
  return m;
}
let a: Delegator = {"delegate": {}};
console.log(operation(a));
a.delegate = {};
console.log(operation(a));
a.delegate = newDelegate();
console.log(operation(a));
