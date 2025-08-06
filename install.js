const https = require('https');
const fs = require('fs');
const path = require('path');
const os = require('os');
const tar = require('tar');
const { HttpsProxyAgent } = require('https-proxy-agent');

const osMap = { darwin: 'Darwin', linux: 'Linux', win32: 'Windows' };
const archMap = { x64: 'x86_64', arm64: 'arm64' };
const platform = osMap[process.platform];
const arch = archMap[process.arch];

if (!platform || !arch) {
  console.error(`Unsupported platform: ${process.platform} ${process.arch}`);
  process.exit(1);
}

const filename = `mochi_${platform}_${arch}.tar.gz`;
const url = `https://github.com/mochilang/mochi/releases/latest/download/${filename}`;
const destDir = path.join(__dirname, 'bin');
const destFile = path.join(destDir, filename);
const binaryName = process.platform === 'win32' ? 'mochi.exe' : 'mochi';

const proxy = process.env.HTTPS_PROXY || process.env.https_proxy || process.env.HTTP_PROXY || process.env.http_proxy;
const agent = proxy ? new HttpsProxyAgent(proxy) : undefined;

fs.mkdirSync(destDir, { recursive: true });

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

async function run() {
  try {
    await download(url, destFile);
    await tar.x({
      file: destFile,
      cwd: destDir,
      gzip: true,
      filter: p => path.basename(p) === binaryName,
      strip: 1,
    });
    fs.unlinkSync(destFile);
    if (process.platform !== 'win32') {
      fs.chmodSync(path.join(destDir, binaryName), 0o755);
    }
    console.log('Mochi binary installed to', destDir);
  } catch (err) {
    console.error('Failed to install Mochi binary:', err.message || err);
    if (proxy) {
      console.error(`Tried to use proxy: ${proxy}`);
    } else {
      console.error('If you are behind a proxy, set the HTTPS_PROXY environment variable.');
    }
    process.exit(1);
  }
}

run();
