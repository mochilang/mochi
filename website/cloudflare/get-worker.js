// Cloudflare Worker for get.mochi-lang.dev.
//
// Serves the Mochi install script directly so users can run:
//   curl -fsSL https://get.mochi-lang.dev | sh
//
// Deploy with:
//   npx wrangler deploy website/cloudflare/get-worker.js \
//     --name mochi-get \
//     --route 'get.mochi-lang.dev/*'
//
// The script content is fetched from the main site at build time of each
// request and cached at the edge.

const SOURCE = "https://mochi-lang.dev/install.sh";

export default {
  async fetch() {
    const upstream = await fetch(SOURCE, {
      cf: { cacheTtl: 300, cacheEverything: true },
    });
    if (!upstream.ok) {
      return new Response("install script unavailable", { status: 502 });
    }
    return new Response(upstream.body, {
      headers: {
        "content-type": "text/x-shellscript; charset=utf-8",
        "cache-control": "public, max-age=300",
        "x-content-type-options": "nosniff",
      },
    });
  },
};
