#!/usr/bin/env node
// Reads every website/docs/mep/mep-NNNN.md, extracts the YAML
// frontmatter, and writes website/src/data/meps.json. The MEP index
// page imports the JSON so any new MEP-NNNN.md file appears in the
// listing the next time `npm run gen:meps` (or `prebuild` / `prestart`)
// runs.

const fs = require('fs');
const path = require('path');
const matter = require('gray-matter');

const root = path.resolve(__dirname, '..');
const mepDir = path.join(root, 'docs', 'mep');
const outFile = path.join(root, 'src', 'data', 'meps.json');

const entries = [];
for (const name of fs.readdirSync(mepDir)) {
  const m = name.match(/^mep-(\d{4})\.md$/);
  if (!m) continue;
  const src = fs.readFileSync(path.join(mepDir, name), 'utf8');
  const fm = matter(src).data;
  if (typeof fm.mep !== 'number') {
    throw new Error(`${name}: missing numeric \`mep\` frontmatter field`);
  }
  entries.push({
    number: fm.mep,
    title: String(fm.title || ''),
    status: String(fm.status || ''),
    type: String(fm.type || ''),
    description: String(fm.description || ''),
    slug: `/docs/mep/mep-${m[1]}`,
  });
}

entries.sort((a, b) => a.number - b.number);

fs.mkdirSync(path.dirname(outFile), { recursive: true });
fs.writeFileSync(outFile, JSON.stringify(entries, null, 2) + '\n');

console.log(`wrote ${entries.length} MEP entries to ${path.relative(root, outFile)}`);
