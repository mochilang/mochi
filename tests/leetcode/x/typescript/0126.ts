import * as fs from 'fs';

function ladders(begin: string, end: string, words: string[]): string[][] {
  const wordSet = new Set(words);
  if (!wordSet.has(end)) return [];
  const parents = new Map<string, string[]>();
  let level = new Set<string>([begin]);
  const visited = new Set<string>([begin]);
  let found = false;
  while (level.size > 0 && !found) {
    const nextLevel = new Set<string>();
    for (const word of Array.from(level).sort()) {
      const chars = word.split('');
      for (let i = 0; i < chars.length; i++) {
        const orig = chars[i];
        for (let c = 97; c <= 122; c++) {
          const ch = String.fromCharCode(c);
          if (ch === orig) continue;
          chars[i] = ch;
          const nw = chars.join('');
          if (!wordSet.has(nw) || visited.has(nw)) continue;
          nextLevel.add(nw);
          if (!parents.has(nw)) parents.set(nw, []);
          parents.get(nw)!.push(word);
          if (nw === end) found = true;
        }
        chars[i] = orig;
      }
    }
    for (const w of nextLevel) visited.add(w);
    level = nextLevel;
  }
  if (!found) return [];
  const out: string[][] = [];
  const path = [end];
  const backtrack = (word: string) => {
    if (word === begin) {
      out.push([...path].reverse());
      return;
    }
    const plist = [...(parents.get(word) || [])].sort();
    for (const p of plist) {
      path.push(p);
      backtrack(p);
      path.pop();
    }
  };
  backtrack(end);
  out.sort((a, b) => a.join('->').localeCompare(b.join('->')));
  return out;
}
function fmt(paths: string[][]): string {
  const lines = [String(paths.length)];
  for (const p of paths) lines.push(p.join('->'));
  return lines.join('\n');
}
const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0] !== '') {
  let idx = 1;
  const tc = parseInt(lines[0], 10);
  const out: string[] = [];
  for (let t = 0; t < tc; t++) {
    const begin = lines[idx++];
    const end = lines[idx++];
    const n = parseInt(lines[idx++], 10);
    const words = lines.slice(idx, idx + n); idx += n;
    out.push(fmt(ladders(begin, end, words)));
  }
  process.stdout.write(out.join('\n\n'));
}
