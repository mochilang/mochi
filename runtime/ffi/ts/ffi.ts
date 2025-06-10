export type FfiFunction = (...args: any[]) => any | Promise<any>;

export interface Caller {
  call(name: string, ...args: any[]): Promise<any>;
}

export interface Registerer {
  register(name: string, fn: FfiFunction): void;
}

export interface Loader {
  loadModule(path: string): Promise<void>;
}

export class Runtime implements Caller, Registerer, Loader {
  private registry: Record<string, FfiFunction> = {};

  register(name: string, fn: FfiFunction): void {
    this.registry[name] = fn;
  }

  async call(name: string, ...args: any[]): Promise<any> {
    const fn = this.registry[name];
    if (!fn) {
      throw new Error(`unknown ffi function: ${name}`);
    }
    return await fn(...args);
  }

  async loadModule(path: string): Promise<void> {
    await import(path);
  }
}

const defaultRuntime = new Runtime();

export function register(name: string, fn: FfiFunction): void {
  defaultRuntime.register(name, fn);
}

export function call(name: string, ...args: any[]): Promise<any> {
  return defaultRuntime.call(name, ...args);
}

export function loadModule(path: string): Promise<void> {
  return defaultRuntime.loadModule(path);
}
