#!/usr/bin/env node
const { spawn } = require('child_process');
const path = require('path');
const fs = require('fs');

const binaryName = process.platform === 'win32' ? 'mochi.exe' : 'mochi';
const bin = path.join(__dirname, 'bin', binaryName);
if (!fs.existsSync(bin)) {
  console.error('Mochi binary not found. Please run `npm install` again.');
  process.exit(1);
}

const args = process.argv.slice(2);
const proc = spawn(bin, args, { stdio: 'inherit' });
proc.on('exit', code => process.exit(code));
proc.on('error', err => {
  console.error('Failed to start Mochi binary:', err);
  process.exit(1);
});

