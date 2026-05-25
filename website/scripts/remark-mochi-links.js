// Remark plugin that auto-links Mochi cross-references in MEP and
// research markdown content. Two patterns are recognised:
//
//   [[name]]    -> link to a sibling research note or a MEP doc.
//                  - [[mep-NNNN]] resolves to /docs/mep/mep-NNNN.
//                  - [[NN-slug]] or [[slug]] inside a research note
//                    (docs/research/BBBB/...) resolves to
//                    /docs/research/BBBB/slug, with any leading
//                    "NN-" / "NN_" stripped because Docusaurus strips
//                    the same prefix when computing the doc id (see
//                    scripts/gen-research.js).
//                  - [[NN-slug]] or [[slug]] inside a MEP body
//                    (docs/mep/mep-NNNN.md) resolves to the matching
//                    file in the paired bundle /docs/research/NNNN/
//                    so the cross-link block at the bottom of each
//                    MEP body becomes clickable.
//
//   MEP-NN      -> link to /docs/mep/mep-NNNN (zero-padded). Matches
//                  MEP-1 through MEP-9999. The \b after the digits
//                  keeps "MEP-49a" or "MEP-49foo" out of scope. The
//                  trailing "+" form ("MEP-50+ territory") and any
//                  reference to a MEP number that does not have a
//                  corresponding mep-NNNN.md file are left as plain
//                  text so forward-looking prose does not introduce
//                  broken links.
//
// Code blocks, inline code, existing link text/urls, link references,
// image alt text, html, and yaml are skipped, so the rewrite is
// idempotent and does not double-link.
//
// Scope: only files under docs/mep/ or docs/research/ are touched.
// Other docs (manual, reference, implementation) are left as-is to
// avoid surprises if those happen to mention "MEP-NN" in passing.

const fs = require('fs');
const path = require('path');

// "MEP-50+" -> the trailing + is a future-marker; we skip those.
const MEP_REF = /\bMEP-(\d{1,4})(?!\+)\b/g;
const WIKILINK = /\[\[([A-Za-z0-9][A-Za-z0-9_-]*)\]\]/g;

let validMepSet = null;

function loadValidMeps() {
  if (validMepSet !== null) return validMepSet;
  validMepSet = new Set();
  // First try the pre-generated meps.json (fast path).
  const jsonPath = path.resolve(__dirname, '..', 'src', 'data', 'meps.json');
  if (fs.existsSync(jsonPath)) {
    try {
      const data = JSON.parse(fs.readFileSync(jsonPath, 'utf8'));
      for (const entry of data) {
        if (typeof entry.number === 'number') {
          validMepSet.add(String(entry.number).padStart(4, '0'));
        }
      }
      if (validMepSet.size > 0) return validMepSet;
    } catch (_) {
      // fall through to filesystem scan
    }
  }
  // Fallback: scan docs/mep/ directly. Useful before `gen-meps.js`
  // has populated meps.json on a cold build.
  const mepDir = path.resolve(__dirname, '..', 'docs', 'mep');
  if (fs.existsSync(mepDir)) {
    for (const name of fs.readdirSync(mepDir)) {
      const m = name.match(/^mep-(\d{4})\.mdx?$/);
      if (m) validMepSet.add(m[1]);
    }
  }
  return validMepSet;
}

function isValidMep(padded) {
  const set = loadValidMeps();
  return set.has(padded);
}

const SKIP_NODE_TYPES = new Set([
  'link',
  'linkReference',
  'code',
  'inlineCode',
  'definition',
  'image',
  'imageReference',
  'html',
  'yaml',
  'jsx',
  'mdxJsxFlowElement',
  'mdxJsxTextElement',
  'mdxFlowExpression',
  'mdxTextExpression',
]);

function normaliseSep(p) {
  return p.split('\\').join('/');
}

function targetFromPath(filePath) {
  if (!filePath) return null;
  const p = normaliseSep(filePath);
  let m = p.match(/\/docs\/research\/(\d{4})\//);
  if (m) return { kind: 'research', bundle: m[1] };
  m = p.match(/\/docs\/mep\/mep-(\d{4})\.mdx?$/);
  if (m) return { kind: 'mep', bundle: m[1] };
  return null;
}

function makeLink(url, label) {
  return {
    type: 'link',
    url,
    title: null,
    children: [{ type: 'text', value: label }],
  };
}

function resolveWikilink(slug, ctx) {
  let mep = slug.match(/^mep-(\d{4})$/i);
  if (mep) {
    if (!isValidMep(mep[1])) return null;
    return { url: `/docs/mep/mep-${mep[1]}`, label: slug };
  }
  // Accept short MEP refs like [[MEP-45]] or [[mep-9]] and zero-pad.
  mep = slug.match(/^mep-(\d{1,3})$/i);
  if (mep) {
    const padded = mep[1].padStart(4, '0');
    if (!isValidMep(padded)) return null;
    return { url: `/docs/mep/mep-${padded}`, label: slug };
  }
  if (!ctx) return null;
  const stripped = slug.replace(/^\d+[-_]/, '');
  return {
    url: `/docs/research/${ctx.bundle}/${stripped}`,
    label: slug,
  };
}

function rewriteText(node, parent, index, ctx) {
  const text = node.value;
  if (!text) return 0;
  if (!text.includes('[[') && !/MEP-\d/.test(text)) return 0;

  const matches = [];
  let m;
  WIKILINK.lastIndex = 0;
  while ((m = WIKILINK.exec(text)) !== null) {
    matches.push({
      kind: 'wikilink',
      start: m.index,
      end: m.index + m[0].length,
      slug: m[1],
      raw: m[0],
    });
  }
  MEP_REF.lastIndex = 0;
  while ((m = MEP_REF.exec(text)) !== null) {
    matches.push({
      kind: 'mep',
      start: m.index,
      end: m.index + m[0].length,
      num: m[1],
      raw: m[0],
    });
  }
  if (matches.length === 0) return 0;
  matches.sort((a, b) => a.start - b.start);

  const out = [];
  let cursor = 0;
  let lastEnd = -1;

  for (const mm of matches) {
    if (mm.start < lastEnd) continue;
    if (mm.start > cursor) {
      out.push({ type: 'text', value: text.slice(cursor, mm.start) });
    }
    let link = null;
    if (mm.kind === 'wikilink') {
      const r = resolveWikilink(mm.slug, ctx);
      if (r) link = makeLink(r.url, r.label);
    } else if (mm.kind === 'mep') {
      const padded = mm.num.padStart(4, '0');
      if (isValidMep(padded)) {
        link = makeLink(`/docs/mep/mep-${padded}`, mm.raw);
      }
    }
    out.push(link || { type: 'text', value: mm.raw });
    cursor = mm.end;
    lastEnd = mm.end;
  }
  if (cursor < text.length) {
    out.push({ type: 'text', value: text.slice(cursor) });
  }

  parent.children.splice(index, 1, ...out);
  return out.length - 1;
}

function walk(node, parent, index, ctx) {
  if (!node || typeof node !== 'object') return 0;
  if (SKIP_NODE_TYPES.has(node.type)) return 0;
  if (node.type === 'text' && parent) {
    return rewriteText(node, parent, index, ctx);
  }
  if (Array.isArray(node.children)) {
    let i = 0;
    while (i < node.children.length) {
      const delta = walk(node.children[i], node, i, ctx);
      i += 1 + delta;
    }
  }
  return 0;
}

function filePathFromVFile(file) {
  if (!file) return null;
  if (file.path) return file.path;
  if (Array.isArray(file.history) && file.history.length > 0) {
    return file.history[file.history.length - 1];
  }
  return null;
}

module.exports = function remarkMochiLinks() {
  return (tree, file) => {
    const ctx = targetFromPath(filePathFromVFile(file));
    if (!ctx) return;
    walk(tree, null, 0, ctx);
  };
};
