#!/usr/bin/env node
// Reads every website/docs/research/NNNN/ subdirectory and writes:
//
//   website/src/data/research-sidebar.json  - grouped sidebar consumed by sidebars.js
//   website/src/data/research-bundles.json  - flat list used by the research index page
//
// Both files are regenerated on `npm run gen:research` (also wired into
// `prebuild` and `prestart`), so dropping a new `docs/research/NNNN/*.md`
// file makes the note appear in both the index page and the left nav on
// the next build without further edits.
//
// File ordering within a bundle is by the filename's leading NN- numeric
// prefix (the same convention used in ~/notes/Spec/NNNN/). Files without
// a leading numeric prefix get sorted alphabetically at the end.
//
// Bundle ordering is by the directory's NNNN number (matching the MEP
// number it feeds).

const fs = require('fs');
const path = require('path');

const root = path.resolve(__dirname, '..');
const researchDir = path.join(root, 'docs', 'research');
const sidebarOut = path.join(root, 'src', 'data', 'research-sidebar.json');
const bundlesOut = path.join(root, 'src', 'data', 'research-bundles.json');

function readH1(filePath) {
  const src = fs.readFileSync(filePath, 'utf8');
  let body = src;
  if (body.startsWith('---\n')) {
    const end = body.indexOf('\n---\n', 4);
    if (end > 0) body = body.slice(end + 5);
  }
  const m = body.match(/^\s*#\s+(.+?)\s*$/m);
  return m ? m[1].trim() : null;
}

function readFrontmatterField(filePath, field) {
  const src = fs.readFileSync(filePath, 'utf8');
  if (!src.startsWith('---\n')) return null;
  const end = src.indexOf('\n---\n', 4);
  if (end < 0) return null;
  const fm = src.slice(4, end);
  const re = new RegExp('^' + field + ':\\s*(.+?)\\s*$', 'm');
  const m = fm.match(re);
  if (!m) return null;
  let val = m[1].trim();
  if ((val.startsWith('"') && val.endsWith('"')) ||
      (val.startsWith("'") && val.endsWith("'"))) {
    val = val.slice(1, -1);
  }
  return val;
}

if (!fs.existsSync(researchDir)) {
  fs.mkdirSync(path.dirname(sidebarOut), { recursive: true });
  fs.writeFileSync(sidebarOut, '[]\n');
  fs.writeFileSync(bundlesOut, '[]\n');
  console.log(`no docs/research/ directory; wrote empty sidebar`);
  return;
}

const bundles = [];

for (const name of fs.readdirSync(researchDir).sort()) {
  const m = name.match(/^(\d{4})$/);
  if (!m) continue;
  const bundleDir = path.join(researchDir, name);
  const stat = fs.statSync(bundleDir);
  if (!stat.isDirectory()) continue;

  const number = parseInt(m[1], 10);
  const files = [];
  for (const f of fs.readdirSync(bundleDir)) {
    if (!f.endsWith('.md')) continue;
    const full = path.join(bundleDir, f);
    const stem = f.replace(/\.md$/, '');
    const numMatch = stem.match(/^(\d+)/);
    const order = numMatch ? parseInt(numMatch[1], 10) : 999;
    const isIndex = stem === 'index';
    // Docusaurus strips a leading "NN-" or "NN_" from the filename when
    // computing the doc id. Mirror that here so the sidebar refs match.
    const docStem = stem.replace(/^\d+[-_]/, '');
    const label =
      readFrontmatterField(full, 'sidebar_label') ||
      readFrontmatterField(full, 'title') ||
      readH1(full) ||
      stem;
    files.push({
      stem,
      order: isIndex ? -1 : order,
      isIndex,
      label,
      docId: `research/${name}/${docStem}`,
    });
  }
  files.sort((a, b) => a.order - b.order || a.stem.localeCompare(b.stem));

  const title =
    readFrontmatterField(path.join(bundleDir, 'index.md'), 'title') ||
    `MEP-${number} research notes`;

  bundles.push({
    number,
    padded: m[1],
    title,
    slug: `/docs/research/${name}/`,
    files,
  });
}

bundles.sort((a, b) => a.number - b.number);

const sidebar = bundles.map(b => ({
  label: `MEP-${b.padded}`,
  items: b.files.map(f => f.docId),
}));

fs.mkdirSync(path.dirname(sidebarOut), { recursive: true });
fs.writeFileSync(sidebarOut, JSON.stringify(sidebar, null, 2) + '\n');
console.log(`wrote ${sidebar.length} research bundle(s) to ${path.relative(root, sidebarOut)}`);

fs.writeFileSync(
  bundlesOut,
  JSON.stringify(
    bundles.map(({ padded, ...rest }) => rest),
    null,
    2,
  ) + '\n',
);
console.log(`wrote ${bundles.length} bundle entries to ${path.relative(root, bundlesOut)}`);
