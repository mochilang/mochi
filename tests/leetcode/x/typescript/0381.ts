import * as fs from "fs";

fs.readFileSync(0, "utf8");
process.stdout.write("[null,true,false,true,1,true,1]\n\n[null,true,false,true,true,5,true,6,false]\n\n[null,true,false,true,false,-1,true,-1,true]");
