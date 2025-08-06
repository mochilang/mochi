#!/usr/bin/env node
const { spawn } = require('child_process');
const path = require('path');
const fs = require('fs');
const https = require('https');
const tar = require('tar');
const { HttpsProxyAgent } = require('https-proxy-agent');

const binaryName = process.platform === 'win32' ? 'mochi.exe' : 'mochi';
const binDir = path.join(__dirname, 'bin');
const bin = path.join(binDir, binaryName);

const proxy = process.env.HTTPS_PROXY || process.env.https_proxy || process.env.HTTP_PROXY || process.env.http_proxy;
const agent = proxy ? new HttpsProxyAgent(proxy) : undefined;

function download(url, outputPath) {
  return new Promise((resolve, reject) => {
    const options = {};
    if (agent) options.agent = agent;
    https.get(url, options, res => {
      if (res.statusCode && res.statusCode >= 300 && res.statusCode < 400 && res.headers.location) {
        download(res.headers.location, outputPath).then(resolve).catch(reject);
        return;
      }
      if (res.statusCode !== 200) {
        reject(new Error(`Failed to download ${url}: ${res.statusCode}`));
        return;
      }
      const file = fs.createWriteStream(outputPath);
      res.pipe(file);
      file.on('finish', () => file.close(resolve));
    }).on('error', reject);
  });
}

async function ensureBinary() {
  if (fs.existsSync(bin)) return;

  const osMap = { darwin: 'Darwin', linux: 'Linux', win32: 'Windows' };
  const archMap = { x64: 'x86_64', arm64: 'arm64' };
  const platform = osMap[process.platform];
  const arch = archMap[process.arch];

  if (!platform || !arch) {
    console.error(`Unsupported platform: ${process.platform} ${process.arch}`);
    process.exit(1);
  }

  fs.mkdirSync(binDir, { recursive: true });
  const filename = `mochi_${platform}_${arch}.tar.gz`;
  const url = `https://github.com/mochilang/mochi/releases/latest/download/${filename}`;
  const destFile = path.join(binDir, filename);

  await download(url, destFile);
  await tar.x({
    file: destFile,
    cwd: binDir,
    gzip: true,
    filter: p => path.basename(p) === binaryName,
    strip: 1,
  });
  fs.unlinkSync(destFile);
  if (process.platform !== 'win32') {
    fs.chmodSync(bin, 0o755);
  }
}

(async () => {
  try {
    await ensureBinary();
    const args = process.argv.slice(2);
    const proc = spawn(bin, args, { stdio: 'inherit' });
    proc.on('exit', code => process.exit(code));
    proc.on('error', err => {
      console.error('Failed to start Mochi binary:', err);
      process.exit(1);
    });
  } catch (err) {
    console.error('Failed to install Mochi binary:', err.message || err);
    if (proxy) {
      console.error(`Tried to use proxy: ${proxy}`);
    } else {
      console.error('If you are behind a proxy, set the HTTPS_PROXY environment variable.');
    }
    process.exit(1);
  }
})();

