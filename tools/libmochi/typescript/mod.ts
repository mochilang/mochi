export interface RunOptions {
  /** Path to the `mochi` executable. Defaults to `mochi`. */
  binary?: string;
  /** Working directory for the process. */
  cwd?: string;
  /** Additional environment variables. */
  env?: Record<string, string>;
}

async function runCommand(args: string[], opts: RunOptions = {}): Promise<string> {
  const cmd = new Deno.Command(opts.binary ?? "mochi", {
    args,
    cwd: opts.cwd,
    env: opts.env,
    stdout: "piped",
    stderr: "piped",
  });
  const { code, stdout, stderr } = await cmd.output();
  const outStr = new TextDecoder().decode(stdout);
  const errStr = new TextDecoder().decode(stderr);
  if (code !== 0) {
    throw new Error(errStr || `mochi exited with code ${code}`);
  }
  return outStr;
}

export async function runFile(path: string, opts?: RunOptions): Promise<string> {
  return await runCommand(["run", path], opts);
}

export async function run(source: string, opts?: RunOptions): Promise<string> {
  const file = await Deno.makeTempFile({ suffix: ".mochi" });
  await Deno.writeTextFile(file, source);
  try {
    return await runFile(file, opts);
  } finally {
    await Deno.remove(file);
  }
}

