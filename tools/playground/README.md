# Mochi Playground

This directory contains a small web-based playground to run Mochi code directly in the browser. The runtime is compiled to WebAssembly using the TinyGo compiler so all execution happens inside the `mochi.wasm` module.

## Building

Install [TinyGo](https://tinygo.org/) and run:

```bash
make build
```

This compiles `mochi.wasm` and copies `wasm_exec.js` from your Go installation.
You can then start a simple HTTP server with:

```bash
make serve
```

Open `index.html` in a browser and start experimenting. The styling uses Tailwind CSS classes inspired by the [shadcn](https://ui.shadcn.com) design system.

## Deployment

The playground can be deployed as a static site using Cloudflare Pages or GitHub Pages:

```bash
make deploy-cloudflare   # publish to Cloudflare Pages
make deploy-gh-pages     # publish to GitHub Pages
```

It is currently available at <https://play.mochi-lang.org> using Cloudflare Pages.
