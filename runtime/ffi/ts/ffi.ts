export type FfiFunction = (...args: any[]) => any | Promise<any>;
export type FfiValue = any;

export class Runtime {
  private registry: Record<string, FfiValue> = {};
  private modules: Record<string, any> = {};

  register(name: string, value: FfiValue): void {
    this.registry[name] = value;
  }

  async call(name: string, ...args: any[]): Promise<any> {
    const value = this.registry[name];
    if (value === undefined) {
      throw new Error(`unknown ffi symbol: ${name}`);
    }
    if (typeof value === 'function') {
      return await (value as FfiFunction)(...args);
    }
    if (args.length === 0) {
      return value;
    }
    throw new Error(`ffi symbol ${name} is not callable`);
  }

  async loadModule(path: string): Promise<void> {
    const mod = await import(path);
    this.modules[path] = mod;
    const entries = Object.entries(mod);
    if (entries.length === 1 && entries[0][0] === 'default' && typeof mod.default === 'object') {
      for (const [k, v] of Object.entries(mod.default)) {
        this.register(k, v);
      }
      return;
    }
    for (const [k, v] of entries) {
      this.register(k, v);
    }
  }

  listPackages(): PackageInfo[] {
    const out: PackageInfo[] = [];
    for (const [path, mod] of Object.entries(this.modules)) {
      let exportsObj: Record<string, any> = mod;
      const entries = Object.entries(mod);
      if (entries.length === 1 && entries[0][0] === 'default' && typeof mod.default === 'object') {
        exportsObj = mod.default as Record<string, any>;
      }
      const exportsInfo = Object.entries(exportsObj).map(([name, value]) => ({
        name,
        type: typeof value,
      }));
      out.push({ path, exports: exportsInfo });
    }
    return out;
  }
}

const defaultRuntime = new Runtime();

export function register(name: string, value: FfiValue): void {
  defaultRuntime.register(name, value);
}

export function call(name: string, ...args: any[]): Promise<any> {
  return defaultRuntime.call(name, ...args);
}

export function loadModule(path: string): Promise<void> {
  return defaultRuntime.loadModule(path);
}

export function listPackages(): PackageInfo[] {
  return defaultRuntime.listPackages();
}

export interface PackageInfo {
  path: string;
  exports: { name: string; type: string }[];
}
