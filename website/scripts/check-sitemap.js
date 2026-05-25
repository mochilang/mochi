#!/usr/bin/env node
// Verifies that the generated sitemap lists every public HTML route.

const fs = require('fs');
const path = require('path');
const config = require('../docusaurus.config');

const root = path.resolve(__dirname, '..');
const buildDir = path.join(root, 'build');
const sitemapPath = path.join(buildDir, 'sitemap.xml');
const ignoredRoutes = new Set(['/404']);

function walk(dir) {
  const out = [];
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      out.push(...walk(full));
    } else if (entry.isFile()) {
      out.push(full);
    }
  }
  return out;
}

function routeFromHtml(filePath) {
  const rel = path.relative(buildDir, filePath).split(path.sep).join('/');
  let route = '/' + rel.replace(/\.html$/, '');
  route = route.replace(/\/index$/, '/');
  return route === '/' ? '/' : route.replace(/\/$/, '');
}

function normalizeSitemapRoute(loc) {
  const siteUrl = String(config.url || '').replace(/\/$/, '');
  const baseUrl = String(config.baseUrl || '/');
  if (!loc.startsWith(siteUrl)) return null;

  let route = loc.slice(siteUrl.length) || '/';
  if (baseUrl !== '/' && route.startsWith(baseUrl)) {
    route = route.slice(baseUrl.length - 1);
  }
  return route === '/' ? '/' : route.replace(/\/$/, '');
}

if (!fs.existsSync(sitemapPath)) {
  throw new Error('build/sitemap.xml does not exist; run npm run build first');
}

const htmlRoutes = new Set(
  walk(buildDir)
    .filter(file => file.endsWith('.html'))
    .map(routeFromHtml)
    .filter(route => !ignoredRoutes.has(route)),
);

const sitemapXml = fs.readFileSync(sitemapPath, 'utf8');
const sitemapRoutes = new Set(
  [...sitemapXml.matchAll(/<loc>([^<]+)<\/loc>/g)]
    .map(match => normalizeSitemapRoute(match[1]))
    .filter(Boolean),
);

const missing = [...htmlRoutes].filter(route => !sitemapRoutes.has(route)).sort();
const stale = [...sitemapRoutes].filter(route => !htmlRoutes.has(route)).sort();

if (missing.length || stale.length) {
  if (missing.length) {
    console.error('Sitemap is missing generated route(s):');
    for (const route of missing) console.error(`  ${route}`);
  }
  if (stale.length) {
    console.error('Sitemap contains route(s) without generated HTML:');
    for (const route of stale) console.error(`  ${route}`);
  }
  process.exit(1);
}

console.log(`sitemap covers ${sitemapRoutes.size} generated route(s)`);
