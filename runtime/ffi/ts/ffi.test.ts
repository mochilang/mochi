import assert from 'assert';
import path from 'path';
import { register, call, loadModule } from './ffi';

(async () => {
  register('add', (a: number, b: number) => a + b);
  const sum = await call('add', 2, 3);
  assert.strictEqual(sum, 5);

  register('answer', 42);
  const ans = await call('answer');
  assert.strictEqual(ans, 42);

  const dir = __dirname;
  await loadModule(path.join(dir, 'sample-module.mjs'));
  const pi = await call('pi');
  assert.strictEqual(pi, 3.14);

  const sq = await call('square', 4);
  assert.strictEqual(sq, 16);

  try {
    await call('pi', 1);
    assert.fail('expected error');
  } catch (err: any) {
    assert.ok((err as Error).message.includes('not callable'));
  }

  console.log('all tests passed');
})();
