// JS companion for js_mul_ints fixture (Phase 10.4).
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
    if (fn === 'js_mul') {
        const result = BigInt(args[0]) * BigInt(args[1]);
        process.stdout.write(JSON.stringify({result: Number(result)}) + '\n');
    } else {
        process.stdout.write(JSON.stringify({error: `unknown function ${fn}`}) + '\n');
    }
});
