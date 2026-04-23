import * as fs from "fs";
function findItinerary(tickets: string[][]): string[] { const graph = new Map<string, string[]>(); for (const [a,b] of tickets) { if (!graph.has(a)) graph.set(a, []); graph.get(a)!.push(b); } for (const v of graph.values()) v.sort().reverse(); const route: string[] = []; function visit(a: string) { const dests = graph.get(a); while (dests && dests.length > 0) visit(dests.pop()!); route.push(a); } visit("JFK"); return route.reverse(); }
function fmt(r: string[]): string { return `[${r.map(s => `"${s}"`).join(",")}]`; }
const data = fs.readFileSync(0, "utf8").trim().split(/\s+/).filter(Boolean);
if (data.length > 0) { let idx = 0; const t = Number(data[idx++]); const out: string[] = []; for (let tc = 0; tc < t; tc++) { const m = Number(data[idx++]); const tickets: string[][] = []; for (let i = 0; i < m; i++) tickets.push([data[idx++], data[idx++]]); out.push(fmt(findItinerary(tickets))); } process.stdout.write(out.join("\n\n")); }
