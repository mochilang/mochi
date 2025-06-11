import "./wasm_exec.js";

const go = new (globalThis as any).Go();
let ready: Promise<void> | null = null;

async function buildWasm(out: URL): Promise<void> {
  const tmp = await Deno.makeTempFile({ suffix: ".wasm" });
  const srcDir = new URL("../../wasm/", import.meta.url);
  const cmd = new Deno.Command("go", {
    args: ["build", "-o", tmp, "main.go"],
    cwd: srcDir.pathname,
    env: { ...Deno.env.toObject(), GOOS: "js", GOARCH: "wasm" },
  });
  const { code, stderr } = await cmd.output();
  if (code !== 0) {
    throw new Error(new TextDecoder().decode(stderr));
  }
  const raw = await Deno.readFile(tmp);
  const gz = new Blob([raw]).stream().pipeThrough(
    new CompressionStream("gzip"),
  );
  const data = new Uint8Array(await new Response(gz).arrayBuffer());
  await Deno.writeFile(out, data);
  await Deno.remove(tmp);
}

async function loadWasm(path: URL): Promise<Uint8Array> {
  try {
    const compressed = await Deno.readFile(path);
    const decompressed = new Blob([compressed]).stream().pipeThrough(
      new DecompressionStream("gzip"),
    );
    return new Uint8Array(await new Response(decompressed).arrayBuffer());
  } catch {
    await buildWasm(path);
    return await loadWasm(path);
  }
}

async function init(): Promise<void> {
  if (!ready) {
    const wasmPath = new URL("./mochi.wasm.gz", import.meta.url);
    const wasmData = await loadWasm(wasmPath);
    const result = await WebAssembly.instantiate(wasmData, go.importObject);
    go.run(result.instance);
    ready = Promise.resolve();
  }
  await ready;
}

export async function run(source: string): Promise<string> {
  await init();
  return String((globalThis as any).runMochi(source));
}

export async function runFile(path: string): Promise<string> {
  const src = await Deno.readTextFile(path);
  return await run(src);
}
