#!/usr/bin/env node
const { spawn } = require('child_process');
const path = require('path');
const fs = require('fs');

const bin = path.join(__dirname, 'bin', process.platform === 'win32' ? 'mochi.exe' : 'mochi');
if (!fs.existsSync(bin)) {
  console.error('Mochi binary not found. Please run `npm install` again.');
  process.exit(1);
}

const args = process.argv.slice(2);
const proc = spawn(bin, args, { stdio: 'inherit' });
proc.on('exit', code => process.exit(code));

