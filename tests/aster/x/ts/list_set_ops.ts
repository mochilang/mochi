// Generated by Mochi v0.10.35 on 2025-07-22 14:07:07 GMT+7
console.log(String("[" + (Array.from(new Set([...[1, 2], ...[2, 3]]))).join(", ") + "]"));
console.log(String("[" + ([1, 2, 3].filter(x => ![2].includes(x))).join(", ") + "]"));
console.log(String("[" + ([1, 2, 3].filter(x => [2, 4].includes(x))).join(", ") + "]"));
console.log(String((Array.isArray([...[1, 2], ...[2, 3]]) || typeof [...[1, 2], ...[2, 3]] === "string" ? [...[1, 2], ...[2, 3]].length : Object.keys([...[1, 2], ...[2, 3]] ?? {}).length)));
