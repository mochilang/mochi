// Generated by Mochi v0.10.35 on 2025-07-22 14:07:02 GMT+7
export interface Result { name: string; age: number }
export interface People { name: string; age: number }
const people: People[] = [{name: "Alice", age: 30}, {name: "Bob", age: 15}, {name: "Charlie", age: 65}, {name: "Diana", age: 45}];
const adults: Result[] = (() => {
    const result: Result[] = [];
    for (const person of people) {
        if (((person["age"] >= 18))) {
            result.push({name: person["name"], age: person["age"], is_senior: (person["age"] >= 60)});
        }
    }
    const out = result;
    return out;
})();
console.log(String("--- Adults ---"));
for (const person of adults) {
    console.log((String(person["name"]) + " " + String("is") + " " + String(person["age"]) + " " + String((person["is_senior"] ? " (senior)" : ""))).trim());
}
