#!/usr/bin/env node
// Reads every website/docs/implementation/NNNN/ subdirectory and writes:
//
//   website/src/data/implementation-sidebar.json - grouped sidebar consumed by sidebars.js
//
// Regenerated on `npm run gen:implementation` (also wired into `prebuild`
// and `prestart`), so dropping a new `docs/implementation/NNNN/*.md` file
// makes the page appear in the left nav on the next build without
// further edits.
//
// Each bundle contributes two top-level entries to the sidebar: the
// bundle's `index` page (rendered as a heading link) followed by a
// collapsible `MEP-NN phases` category with the phase-* docs sorted by
// filename. Filenames are kept verbatim as docIds (no NN- prefix
// stripping), matching the existing convention used by Docusaurus for
// these pages.

const fs = require('fs');
const path = require('path');

const root = path.resolve(__dirname, '..');
const implDir = path.join(root, 'docs', 'implementation');
const sidebarOut = path.join(root, 'src', 'data', 'implementation-sidebar.json');

if (!fs.existsSync(implDir)) {
  fs.mkdirSync(path.dirname(sidebarOut), { recursive: true });
  fs.writeFileSync(sidebarOut, '[]\n');
  console.log(`no docs/implementation/ directory; wrote empty sidebar`);
  return;
}

const bundles = [];

for (const name of fs.readdirSync(implDir).sort()) {
  const m = name.match(/^(\d{4})$/);
  if (!m) continue;
  const bundleDir = path.join(implDir, name);
  if (!fs.statSync(bundleDir).isDirectory()) continue;

  const number = parseInt(m[1], 10);
  const padded = m[1];

  const phases = [];
  let hasIndex = false;
  for (const f of fs.readdirSync(bundleDir)) {
    if (!f.endsWith('.md')) continue;
    const stem = f.replace(/\.md$/, '');
    if (stem === 'index') {
      hasIndex = true;
      continue;
    }
    phases.push(stem);
  }
  phases.sort();

  bundles.push({ number, padded, hasIndex, phases });
}

bundles.sort((a, b) => a.number - b.number);

const sidebar = [];
for (const b of bundles) {
  if (b.hasIndex) sidebar.push(`implementation/${b.padded}/index`);
  if (b.phases.length === 0) continue;
  sidebar.push({
    label: `MEP-${b.padded.replace(/^0+/, '') || '0'} phases`,
    items: b.phases.map(p => `implementation/${b.padded}/${p}`),
  });
}

fs.mkdirSync(path.dirname(sidebarOut), { recursive: true });
fs.writeFileSync(sidebarOut, JSON.stringify(sidebar, null, 2) + '\n');
console.log(`wrote ${bundles.length} implementation bundle(s) to ${path.relative(root, sidebarOut)}`);
