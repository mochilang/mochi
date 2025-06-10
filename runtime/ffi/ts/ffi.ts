export type FfiFunction = (...args: any[]) => any | Promise<any>;

const registry: Record<string, FfiFunction> = {};

export function register(name: string, fn: FfiFunction): void {
  registry[name] = fn;
}

export async function call(name: string, ...args: any[]): Promise<any> {
  const fn = registry[name];
  if (!fn) {
    throw new Error(`unknown ffi function: ${name}`);
  }
  return await fn(...args);
}

export async function loadModule(path: string): Promise<void> {
  await import(path);
}
