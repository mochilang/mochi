// JS companion for js_str_trim fixture (Phase 10.4).
const readline = require('readline');

const rl = readline.createInterface({ input: process.stdin, terminal: false });

rl.on('line', (line) => {
    line = line.trim();
    if (!line) return;
    let req;
    try {
        req = JSON.parse(line);
    } catch (e) {
        process.stdout.write(JSON.stringify({error: 'parse error'}) + '\n');
        return;
    }
    const fn = req.fn;
    const args = req.args;
    if (fn === 'js_trim') {
        const result = String(args[0]).trim();
        process.stdout.write(JSON.stringify({result}) + '\n');
    } else {
        process.stdout.write(JSON.stringify({error: `unknown function ${fn}`}) + '\n');
    }
});
