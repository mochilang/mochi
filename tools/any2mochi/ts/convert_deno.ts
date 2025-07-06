import { Project, Node } from "npm:ts-morph";

function tsToMochiType(t: string): string {
  t = t.trim();
  if (t.includes("|")) {
    const parts = t.split("|").map(p => p.trim()).filter(p => p !== "null" && p !== "undefined").map(tsToMochiType).filter(Boolean);
    if (parts.length === 1) return parts[0];
    if (parts.length > 1) return "any";
    return "";
  }
  switch (t) {
    case "":
    case "any":
    case "unknown":
    case "object":
      return "";
    case "number":
      return "int";
    case "string":
      return "string";
    case "boolean":
      return "bool";
    case "void":
    case "undefined":
    case "null":
      return "";
  }
  if (t.endsWith("[]")) {
    const inner = tsToMochiType(t.slice(0, -2)) || "any";
    return `list<${inner}>`;
  }
  if (t.startsWith("Array<") && t.endsWith(">")) {
    const inner = tsToMochiType(t.slice(6, -1)) || "any";
    return `list<${inner}>`;
  }
  if (t.startsWith("Record<") && t.endsWith(">")) {
    const [k, v] = t.slice(7, -1).split(/\s*,\s*/);
    const key = tsToMochiType(k || "any") || "any";
    const val = tsToMochiType(v || "any") || "any";
    return `map<${key},${val}>`;
  }
  return t;
}

function findMatch(s: string, openIdx: number, open: string, close: string): number {
  let depth = 0;
  for (let i = openIdx; i < s.length; i++) {
    const r = s[i];
    if (r === open) depth++;
    else if (r === close) {
      depth--;
      if (depth === 0) return i;
    }
  }
  return s.length;
}

function tsFunctionBody(src: string): string[] {
  const lines: string[] = [];
  let s = src.trim();
  while (s.length > 0) {
    s = s.replace(/^\s*[;\n\r]*/, "");
    if (s.length === 0) break;
    if (s.startsWith("break")) {
      const end = s.indexOf(";");
      lines.push("break");
      s = end === -1 ? "" : s.slice(end + 1);
      continue;
    }
    if (s.startsWith("continue")) {
      const end = s.indexOf(";");
      lines.push("continue");
      s = end === -1 ? "" : s.slice(end + 1);
      continue;
    }
    if (s.startsWith("return ")) {
      const end = s.indexOf(";");
      const expr = s.slice(7, end === -1 ? undefined : end).trim();
      lines.push(`return ${expr}`);
      s = end === -1 ? "" : s.slice(end + 1);
      continue;
    }
    if (s.startsWith("console.log(")) {
      const end = s.indexOf(")");
      const expr = s.slice(12, end === -1 ? undefined : end).trim();
      lines.push(`print(${expr})`);
      const sem = s.slice(end).indexOf(";");
      if (sem !== -1) {
        s = s.slice(end + sem + 1);
      } else {
        s = s.slice(end + 1);
      }
      continue;
    }
    if (/^(let|const|var) /.test(s)) {
      const end = s.indexOf(";");
      let stmt = s.slice(0, end === -1 ? undefined : end).trim();
      stmt = stmt.replace(/^let\s+|^const\s+|^var\s+/, "");
      lines.push(`let ${stmt.replace(/;$/, "")}`);
      s = end === -1 ? "" : s.slice(end + 1);
      continue;
    }
    if (s.startsWith("if (")) {
      const condEnd = findMatch(s, s.indexOf("("), "(", ")");
      const cond = s.slice(s.indexOf("(") + 1, condEnd).trim();
      const bodyStartRel = s.slice(condEnd).indexOf("{");
      if (bodyStartRel === -1) { s = s.slice(condEnd); continue; }
      const bodyStart = condEnd + bodyStartRel + 1;
      const bodyEnd = findMatch(s, bodyStart - 1, "{", "}");
      const bodyLines = tsFunctionBody(s.slice(bodyStart, bodyEnd));
      lines.push(`if ${cond} {`);
      for (const l of bodyLines) lines.push(`  ${l}`);
      lines.push("}");
      s = s.slice(bodyEnd + 1).trim();
      if (s.startsWith("else {")) {
        const elseStart = s.indexOf("{") + 1;
        const elseEnd = findMatch(s, elseStart - 1, "{", "}");
        const elseLines = tsFunctionBody(s.slice(elseStart, elseEnd));
        lines.push("else {");
        for (const l of elseLines) lines.push(`  ${l}`);
        lines.push("}");
        s = s.slice(elseEnd + 1);
      }
      continue;
    }
    if (s.startsWith("for (")) {
      const parenEnd = findMatch(s, s.indexOf("("), "(", ")");
      const clause = s.slice(s.indexOf("(") + 1, parenEnd).trim();
      if (clause.includes(" of ")) {
        const [iterRaw, list] = clause.split(" of ", 2);
        let iter = iterRaw.trim();
        iter = iter.replace(/^let\s+|^const\s+|^var\s+/, "");
        const bodyStartRel = s.slice(parenEnd).indexOf("{");
        if (bodyStartRel === -1) { s = s.slice(parenEnd); continue; }
        const bodyStart = parenEnd + bodyStartRel + 1;
        const bodyEnd = findMatch(s, bodyStart - 1, "{", "}");
        const bodyLines = tsFunctionBody(s.slice(bodyStart, bodyEnd));
        lines.push(`for ${iter} in ${list.trim()} {`);
        for (const l of bodyLines) lines.push(`  ${l}`);
        lines.push("}");
        s = s.slice(bodyEnd + 1);
        continue;
      }
      const semi = s.indexOf(";");
      s = semi === -1 ? "" : s.slice(semi + 1);
      continue;
    }
    if (s.startsWith("while (")) {
      const parenEnd = findMatch(s, s.indexOf("("), "(", ")");
      const cond = s.slice(s.indexOf("(") + 1, parenEnd).trim();
      const bodyStartRel = s.slice(parenEnd).indexOf("{");
      if (bodyStartRel === -1) { s = s.slice(parenEnd); continue; }
      const bodyStart = parenEnd + bodyStartRel + 1;
      const bodyEnd = findMatch(s, bodyStart - 1, "{", "}");
      const bodyLines = tsFunctionBody(s.slice(bodyStart, bodyEnd));
      lines.push(`while ${cond} {`);
      for (const l of bodyLines) lines.push(`  ${l}`);
      lines.push("}");
      s = s.slice(bodyEnd + 1);
      continue;
    }
    const semi = s.indexOf(";");
    if (semi !== -1) s = s.slice(semi + 1);
    else s = "";
  }
  return lines;
}

function convert(src: string): string {
  const project = new Project({ useInMemoryFileSystem: true });
  const file = project.createSourceFile("input.ts", src);
  let out = "";

  for (const stmt of file.getStatements()) {
    if (Node.isVariableStatement(stmt)) {
      for (const d of stmt.getDeclarationList().getDeclarations()) {
        const name = d.getName();
        const typ = tsToMochiType(d.getTypeNode()?.getText() || "");
        out += `let ${name}`;
        if (typ) out += `: ${typ}`;
        out += "\n";
      }
    } else if (Node.isFunctionDeclaration(stmt)) {
      const name = stmt.getName() || "";
      out += `fun ${name}(`;
      const params = stmt.getParameters();
      params.forEach((p, i) => {
        if (i > 0) out += ", ";
        out += p.getName();
        const pt = tsToMochiType(p.getTypeNode()?.getText() || "");
        if (pt) out += `: ${pt}`;
      });
      out += ")";
      const rt = tsToMochiType(stmt.getReturnTypeNode()?.getText() || "");
      if (rt && rt !== "void") out += `: ${rt}`;
      const bodyText = stmt.getBodyText() || "";
      const lines = tsFunctionBody(bodyText);
      if (lines.length === 0) out += " {}\n";
      else {
        out += " {\n";
        for (const l of lines) out += `  ${l}\n`;
        out += "}\n";
      }
    } else if (Node.isEnumDeclaration(stmt)) {
      out += `type ${stmt.getName()} {\n`;
      for (const m of stmt.getMembers()) {
        out += `  ${m.getName()}\n`;
      }
      out += "}\n";
    } else if (Node.isClassDeclaration(stmt) || Node.isInterfaceDeclaration(stmt)) {
      const name = stmt.getName() || "";
      out += `type ${name} {\n`;
      for (const mem of stmt.getMembers()) {
        if (Node.isPropertyDeclaration(mem) || Node.isPropertySignature(mem)) {
          const mn = mem.getName();
          const mt = tsToMochiType(mem.getTypeNode()?.getText() || "");
          out += `  ${mn}`;
          if (mt) out += `: ${mt}`;
          out += "\n";
        } else if (Node.isMethodDeclaration(mem) || Node.isMethodSignature(mem) || Node.isConstructorDeclaration(mem)) {
          let mname = mem.getName();
          if (!mname) mname = "constructor";
          let line = `  fun ${mname}(`;
          const ps = mem.getParameters();
          ps.forEach((p, i) => {
            if (i > 0) line += ", ";
            line += p.getName();
            const pt = tsToMochiType(p.getTypeNode()?.getText() || "");
            if (pt) line += `: ${pt}`;
          });
          line += ")";
          const rt = tsToMochiType((mem as any).getReturnTypeNode?.()?.getText() || "");
          if (rt && rt !== "void") line += `: ${rt}`;
          const bodyText = (mem as any).getBodyText?.() || "";
          const lines = tsFunctionBody(bodyText);
          if (lines.length === 0) line += " {}";
          else {
            line += " {";
            for (const l of lines) line += `\n    ${l}`;
            line += "\n  }";
          }
          out += line + "\n";
        }
      }
      out += "}\n";
    } else if (Node.isTypeAliasDeclaration(stmt)) {
      const name = stmt.getName();
      const tn = stmt.getTypeNode();
      if (tn && Node.isTypeLiteral(tn)) {
        out += `type ${name} {\n`;
        for (const mem of tn.getMembers()) {
          if (Node.isPropertySignature(mem)) {
            const mn = mem.getName();
            const mt = tsToMochiType(mem.getTypeNode()?.getText() || "");
            out += `  ${mn}`;
            if (mt) out += `: ${mt}`;
            out += "\n";
          }
        }
        out += "}\n";
      } else if (tn) {
        const alias = tsToMochiType(tn.getText());
        out += `type ${name} = ${alias}\n`;
      }
    }
  }

  return out;
}

if (import.meta.main) {
  const file = Deno.args[0];
  const src = await Deno.readTextFile(file);
  const out = convert(src);
  console.log(out);
}
