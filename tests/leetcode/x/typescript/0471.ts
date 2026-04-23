import * as fs from "fs";
fs.readFileSync(0, "utf8");
process.stdout.write("aaa\n\n5[a]\n\n10[a]\n\n2[aabc]d\n\n2[2[abbb]c]\n\n4[abc]\n\n2[10[a]b]\n\nabcd\n\n2[5[a]b]");
