import { assertEquals } from "https://deno.land/std@0.224.0/assert/mod.ts";
import { run, runFile } from "./mod.ts";

Deno.test("run source string", async () => {
  const out = await run('print("deno")');
  assertEquals(out.trim(), "deno");
});

Deno.test("run file", async () => {
  const path = await Deno.makeTempFile({ suffix: ".mochi" });
  await Deno.writeTextFile(path, 'print("file")');
  try {
    const out = await runFile(path);
    assertEquals(out.trim(), "file");
  } finally {
    await Deno.remove(path);
  }
});

