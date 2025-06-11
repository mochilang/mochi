import { run, runFile } from "./mod.ts";

function assertEquals(actual: unknown, expected: unknown): void {
  if (actual !== expected) {
    throw new Error(`assertEquals failed: ${actual} !== ${expected}`);
  }
}

denoTest('run source string', async () => {
  const out = await run('print("deno")');
  assertEquals(out.trim(), 'deno');
});

denoTest('run file', async () => {
  const path = await Deno.makeTempFile({ suffix: '.mochi' });
  await Deno.writeTextFile(path, 'print("file")');
  try {
    const out = await runFile(path);
    assertEquals(out.trim(), 'file');
  } finally {
    await Deno.remove(path);
  }
});

function denoTest(name: string, fn: () => Promise<void>) {
  Deno.test(name, fn);
}
