#!/usr/bin/env node
// Mirrors root-level releases/vX.Y.Z.md files into Docusaurus docs.
//
// The root releases/ directory stays the single source of truth. This script
// writes:
//
//   website/docs/releases/vX.Y.Z.md       - one doc page per release
//   website/src/data/releases.json        - release metadata for changelog.mdx
//   website/src/data/release-sidebar.json - flat release page refs for sidebars.js

const fs = require('fs');
const path = require('path');

const root = path.resolve(__dirname, '..');
const repoRoot = path.resolve(root, '..');
const sourceDir = path.join(repoRoot, 'releases');
const docsOutDir = path.join(root, 'docs', 'releases');
const dataOut = path.join(root, 'src', 'data', 'releases.json');
const sidebarOut = path.join(root, 'src', 'data', 'release-sidebar.json');

function parseVersion(name) {
  const m = name.match(/^v(\d+)\.(\d+)\.(\d+)\.md$/);
  if (!m) return null;
  return {
    tag: `v${m[1]}.${m[2]}.${m[3]}`,
    major: Number(m[1]),
    minor: Number(m[2]),
    patch: Number(m[3]),
  };
}

function compareVersionsDesc(a, b) {
  return (
    b.major - a.major ||
    b.minor - a.minor ||
    b.patch - a.patch
  );
}

function monthFromTitle(src) {
  const m = src.match(/^#\s+(.+?)\s+\(v\d+\.\d+\.\d+\)\s*$/m);
  return m ? m[1].trim() : '';
}

function firstParagraph(src) {
  const withoutHeading = src.replace(/^#.+(?:\r?\n)+/, '');
  const paragraphs = withoutHeading
    .split(/\r?\n\r?\n/)
    .map(p => p.trim())
    .filter(Boolean)
    .filter(p => !p.startsWith('Released on '))
    .filter(p => !p.startsWith('```'));
  const text = (paragraphs[0] || '').replace(/\s+/g, ' ').trim();
  if (text.length <= 240) return text;
  return text.slice(0, 237).replace(/\s+\S*$/, '') + '...';
}

function frontmatter(release) {
  const title = `${release.tag}${release.month ? ` - ${release.month}` : ''}`;
  return [
    '---',
    `title: ${JSON.stringify(title)}`,
    `description: ${JSON.stringify(release.summary || `Release notes for Mochi ${release.tag}.`)}`,
    `sidebar_label: ${JSON.stringify(release.tag)}`,
    'toc_min_heading_level: 2',
    'toc_max_heading_level: 2',
    '---',
    '',
  ].join('\n');
}

if (!fs.existsSync(sourceDir)) {
  throw new Error(`missing releases directory: ${sourceDir}`);
}

const releases = fs.readdirSync(sourceDir)
  .map(name => {
    const version = parseVersion(name);
    if (!version) return null;
    const src = fs.readFileSync(path.join(sourceDir, name), 'utf8').trim() + '\n';
    return {
      ...version,
      file: name,
      month: monthFromTitle(src),
      summary: firstParagraph(src),
      docId: `releases/${version.tag}`,
      slug: `/docs/releases/${version.tag}`,
      body: src,
    };
  })
  .filter(Boolean)
  .sort(compareVersionsDesc);

fs.rmSync(docsOutDir, { recursive: true, force: true });
fs.mkdirSync(docsOutDir, { recursive: true });

for (const release of releases) {
  fs.writeFileSync(
    path.join(docsOutDir, `${release.tag}.md`),
    frontmatter(release) + release.body,
  );
}

fs.mkdirSync(path.dirname(dataOut), { recursive: true });
fs.writeFileSync(
  dataOut,
  JSON.stringify(
    releases.map(({ body, ...release }) => release),
    null,
    2,
  ) + '\n',
);

fs.writeFileSync(
  sidebarOut,
  JSON.stringify(releases.map(release => release.docId), null, 2) + '\n',
);

console.log(`wrote ${releases.length} release docs to ${path.relative(root, docsOutDir)}`);
console.log(`wrote release metadata to ${path.relative(root, dataOut)}`);
console.log(`wrote ${releases.length} release sidebar entries to ${path.relative(root, sidebarOut)}`);
