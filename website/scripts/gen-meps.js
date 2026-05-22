#!/usr/bin/env node
// Reads every website/docs/mep/mep-NNNN.md, extracts the YAML
// frontmatter, and writes:
//
//   website/src/data/meps.json         - flat list used by /docs/mep
//   website/src/data/mep-sidebar.json  - grouped list used by sidebars.js
//
// Both files are regenerated on `npm run gen:meps` (also wired into
// `prebuild` and `prestart`), so adding a new mep-NNNN.md file makes
// the MEP appear in both the index page and the left nav on the next
// build without further edits.

const fs = require('fs');
const path = require('path');
const matter = require('gray-matter');

const root = path.resolve(__dirname, '..');
const mepDir = path.join(root, 'docs', 'mep');
const listOut = path.join(root, 'src', 'data', 'meps.json');
const sidebarOut = path.join(root, 'src', 'data', 'mep-sidebar.json');

// Sidebar groups for the left nav. Each entry slots MEPs whose number
// falls in [min, max] inclusive. Ranges are contiguous and the final
// entry should extend past the highest MEP number, so a freshly added
// MEP in an existing topic area is auto-categorised. To introduce a
// new topic group (say, "Tooling" starting at MEP-N), insert it here
// and shrink the previous group's max.
//
// MEP 0 (the index of MEPs) is rendered separately above the groups
// so it does not appear in any range below.
const sidebarGroups = [
  { label: 'Foundations',     min: 1,  max: 6 },
  { label: 'Soundness',       min: 7,  max: 10 },
  { label: 'Standards Track', min: 11, max: 16 },
  { label: 'VM & Runtime',    min: 17, max: 29 },
  { label: 'JIT',             min: 30, max: 34 },
  { label: 'Memory',          min: 35, max: 41 },
  { label: 'Native Codegen',  min: 42, max: 42 },
  { label: 'Go Interop',      min: 43, max: 44 },
  { label: 'C Target',        min: 45, max: 45 },
  { label: 'Future',          min: 46, max: 9999 },
];

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
    padded: m[1],
    title: String(fm.title || ''),
    status: String(fm.status || ''),
    type: String(fm.type || ''),
    description: String(fm.description || ''),
    slug: `/docs/mep/mep-${m[1]}`,
  });
}

entries.sort((a, b) => a.number - b.number);

fs.mkdirSync(path.dirname(listOut), { recursive: true });
fs.writeFileSync(
  listOut,
  JSON.stringify(
    entries.map(({ padded, ...rest }) => rest),
    null,
    2,
  ) + '\n',
);
console.log(`wrote ${entries.length} MEP entries to ${path.relative(root, listOut)}`);

const groups = sidebarGroups.map(g => ({ label: g.label, items: [] }));
const overflow = [];
for (const e of entries) {
  if (e.number === 0) continue; // mep-0000 is the hand-placed index
  const i = sidebarGroups.findIndex(g => e.number >= g.min && e.number <= g.max);
  if (i < 0) {
    overflow.push(`mep/mep-${e.padded}`);
    continue;
  }
  groups[i].items.push(`mep/mep-${e.padded}`);
}
if (overflow.length) {
  groups.push({ label: 'Other', items: overflow });
}
const nonEmpty = groups.filter(g => g.items.length > 0);
fs.writeFileSync(sidebarOut, JSON.stringify(nonEmpty, null, 2) + '\n');
console.log(`wrote ${nonEmpty.length} sidebar groups to ${path.relative(root, sidebarOut)}`);
