// Simple TypeScript to Mochi converter using regex based parsing.
// This is a lightweight fallback when the language server is disabled.

export interface TSParam {
  name: string;
  typ: string;
}

export function convert(src: string): string {
  let out = "";
  parseTSFallback(outBuilder => (out += outBuilder), src);
  return out;
}

function parseTSFallback(write: (s: string) => void, src: string) {
  const typeRe = /type\s+([A-Za-z_][A-Za-z0-9_]*)\s*=\s*{/gs;
  let m: RegExpExecArray | null;
  while ((m = typeRe.exec(src))) {
    const name = m[1];
    const open = src.indexOf("{", m.index);
    const close = findMatch(src, open, '{', '}');
    if (close <= open) continue;
    const body = src.slice(open + 1, close);
    write(`type ${name} {\n`);
    for (const line of body.split(/\n/)) {
      const l = line.trim().replace(/;$/, "");
      if (!l) continue;
      const colon = l.indexOf(":");
      if (colon !== -1) {
        const field = l.slice(0, colon).trim();
        const typ = tsToMochiType(l.slice(colon + 1).trim());
        write(`  ${field}`);
        if (typ) write(`: ${typ}`);
        write("\n");
      }
    }
    write("}\n");
  }

  const varRe = /(?:^|\n)(?:let|const|var)\s+([A-Za-z_][A-Za-z0-9_]*)\s*(?::\s*([^=;\n]+))?/g;
  while ((m = varRe.exec(src))) {
    const name = m[1];
    const typ = tsToMochiType((m[2] || "").trim());
    write(`let ${name}`);
    if (typ) write(`: ${typ}`);
    write("\n");
  }

  const funcRe = /function\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)\s*(?::\s*([^\{\n]+))?\s*{/gs;
  while ((m = funcRe.exec(src))) {
    const name = m[1];
    const paramsPart = m[2];
    const ret = (m[3] || "").trim();
    const open = src.indexOf("{", m.index);
    const close = findMatch(src, open, '{', '}');
    let body = "";
    if (close > open) {
      body = src.slice(open + 1, close);
    }
    write(`fun ${name}(`);
    const { params } = parseTSSignature(`(${paramsPart})${funcReturnSig(ret)}`);
    for (let i = 0; i < params.length; i++) {
      if (i > 0) write(", ");
      write(params[i].name);
      if (params[i].typ) write(`: ${params[i].typ}`);
    }
    write(')');
    const mappedRet = tsToMochiType(ret);
    if (mappedRet && mappedRet !== 'void') {
      write(`: ${mappedRet}`);
    }
    const stmts = tsFunctionBody(body);
    if (stmts.length === 0) {
      write(' {}\n');
    } else {
      write(' {\n');
      for (const l of stmts) {
        write(`  ${l}\n`);
      }
      write('}\n');
    }
  }
}

function funcReturnSig(ret: string): string {
  return ret ? `: ${ret}` : '';
}

function parseTSSignature(sig: string): { params: TSParam[]; ret: string } {
  sig = sig.trim();
  const open = sig.indexOf('(');
  const close = sig.lastIndexOf(')');
  if (open === -1 || close === -1 || close < open) {
    return { params: [], ret: tsToMochiType(sig.trim()) };
  }
  const paramsPart = sig.slice(open + 1, close);
  const params: TSParam[] = [];
  for (const p of splitTSParams(paramsPart)) {
    const trimmed = p.trim();
    if (!trimmed) continue;
    let name = trimmed;
    let typ = '';
    const colon = trimmed.indexOf(':');
    if (colon !== -1) {
      name = trimmed.slice(0, colon).trim();
      typ = trimmed.slice(colon + 1).trim();
    }
    params.push({ name, typ: tsToMochiType(typ) });
  }
  let rest = sig.slice(close + 1).trim();
  if (rest.startsWith(':')) rest = rest.slice(1).trim();
  else if (rest.startsWith('=>')) rest = rest.slice(2).trim();
  return { params, ret: tsToMochiType(rest) };
}

function splitTSParams(s: string): string[] {
  const parts: string[] = [];
  let depth = 0;
  let start = 0;
  for (let i = 0; i < s.length; i++) {
    const r = s[i];
    if (r === '<' || r === '(' || r === '[') {
      depth++;
    } else if (r === '>' || r === ')' || r === ']') {
      if (depth > 0) depth--;
    } else if (r === ',' && depth === 0) {
      parts.push(s.slice(start, i));
      start = i + 1;
    }
  }
  if (start < s.length) {
    parts.push(s.slice(start));
  }
  return parts;
}

function tsToMochiType(t: string): string {
  t = t.trim();
  if (t.includes('|')) {
    const parts = t.split('|').map(p => p.trim()).filter(p => p !== 'null' && p !== 'undefined').map(tsToMochiType).filter(Boolean);
    if (parts.length === 1) return parts[0];
    if (parts.length > 1) return 'any';
    return '';
  }
  switch (t) {
    case '':
    case 'any':
    case 'unknown':
    case 'object':
      return '';
    case 'number':
      return 'int';
    case 'string':
      return 'string';
    case 'boolean':
      return 'bool';
    case 'void':
    case 'undefined':
    case 'null':
      return '';
  }
  if (t.endsWith('[]')) {
    const inner = tsToMochiType(t.slice(0, -2)) || 'any';
    return `list<${inner}>`;
  }
  if (t.startsWith('Array<') && t.endsWith('>')) {
    const inner = tsToMochiType(t.slice(6, -1)) || 'any';
    return `list<${inner}>`;
  }
  if (t.startsWith('Record<') && t.endsWith('>')) {
    const parts = splitTSParams(t.slice(7, -1));
    let key = 'any';
    let val = 'any';
    if (parts[0]) {
      const k = tsToMochiType(parts[0]);
      if (k) key = k;
    }
    if (parts[1]) {
      const v = tsToMochiType(parts[1]);
      if (v) val = v;
    }
    return `map<${key},${val}>`;
  }
  return t;
}

function tsFunctionBody(src: string): string[] {
  const lines: string[] = [];
  const uninit: Record<string, boolean> = {};
  let s = src.trim();
  while (s.length > 0) {
    s = s.replace(/^\s*[;\n\r]*/, '');
    if (s.length === 0) break;
    if (s.startsWith('break')) {
      const end = s.indexOf(';');
      lines.push('break');
      s = end === -1 ? '' : s.slice(end + 1);
      continue;
    }
    if (s.startsWith('continue')) {
      const end = s.indexOf(';');
      lines.push('continue');
      s = end === -1 ? '' : s.slice(end + 1);
      continue;
    }
    if (s.startsWith('return ')) {
      const end = s.indexOf(';');
      const expr = s.slice(7, end === -1 ? undefined : end).trim();
      lines.push(`return ${expr}`);
      s = end === -1 ? '' : s.slice(end + 1);
      continue;
    }
    if (s.startsWith('console.log(')) {
      const end = s.indexOf(')');
      const expr = s.slice(12, end === -1 ? undefined : end).trim();
      lines.push(`print(${expr})`);
      const sem = s.slice(end).indexOf(';');
      if (sem !== -1) {
        s = s.slice(end + sem + 1);
      } else {
        s = s.slice(end + 1);
      }
      continue;
    }
    if (/^(let|const|var) /.test(s)) {
      const end = s.indexOf(';');
      let stmt = s.slice(0, end === -1 ? undefined : end).trim();
      stmt = stmt.replace(/^let\s+|^const\s+|^var\s+/, '');
      if (stmt.includes('=')) {
        lines.push(`let ${stmt.replace(/;$/, '')}`);
      } else {
        uninit[stmt] = true;
      }
      s = end === -1 ? '' : s.slice(end + 1);
      continue;
    }
    if (s.startsWith('if (')) {
      const condEnd = findMatch(s, s.indexOf('('), '(', ')');
      const cond = s.slice(s.indexOf('(') + 1, condEnd).trim();
      const bodyStartRel = s.slice(condEnd).indexOf('{');
      if (bodyStartRel === -1) { s = s.slice(condEnd); continue; }
      const bodyStart = condEnd + bodyStartRel + 1;
      const bodyEnd = findMatch(s, bodyStart - 1, '{', '}');
      const bodyLines = tsFunctionBody(s.slice(bodyStart, bodyEnd));
      lines.push(`if ${cond} {`);
      for (const l of bodyLines) lines.push(`  ${l}`);
      lines.push('}');
      s = s.slice(bodyEnd + 1).trim();
      if (s.startsWith('else {')) {
        const elseStart = s.indexOf('{') + 1;
        const elseEnd = findMatch(s, elseStart - 1, '{', '}');
        const elseLines = tsFunctionBody(s.slice(elseStart, elseEnd));
        lines.push('else {');
        for (const l of elseLines) lines.push(`  ${l}`);
        lines.push('}');
        s = s.slice(elseEnd + 1);
      }
      continue;
    }
    if (s.startsWith('for (')) {
      const parenEnd = findMatch(s, s.indexOf('('), '(', ')');
      const clause = s.slice(s.indexOf('(') + 1, parenEnd).trim();
      if (clause.includes(' of ')) {
        const [iterRaw, list] = clause.split(' of ', 2);
        let iter = iterRaw.trim();
        iter = iter.replace(/^let\s+|^const\s+|^var\s+/, '');
        const bodyStartRel = s.slice(parenEnd).indexOf('{');
        if (bodyStartRel === -1) { s = s.slice(parenEnd); continue; }
        const bodyStart = parenEnd + bodyStartRel + 1;
        const bodyEnd = findMatch(s, bodyStart - 1, '{', '}');
        const bodyLines = tsFunctionBody(s.slice(bodyStart, bodyEnd));
        lines.push(`for ${iter} in ${list.trim()} {`);
        for (const l of bodyLines) lines.push(`  ${l}`);
        lines.push('}');
        s = s.slice(bodyEnd + 1);
        continue;
      }
      const semi = s.indexOf(';');
      s = semi === -1 ? '' : s.slice(semi + 1);
      continue;
    }
    if (s.startsWith('while (')) {
      const parenEnd = findMatch(s, s.indexOf('('), '(', ')');
      const cond = s.slice(s.indexOf('(') + 1, parenEnd).trim();
      const bodyStartRel = s.slice(parenEnd).indexOf('{');
      if (bodyStartRel === -1) { s = s.slice(parenEnd); continue; }
      const bodyStart = parenEnd + bodyStartRel + 1;
      const bodyEnd = findMatch(s, bodyStart - 1, '{', '}');
      const bodyLines = tsFunctionBody(s.slice(bodyStart, bodyEnd));
      lines.push(`while ${cond} {`);
      for (const l of bodyLines) lines.push(`  ${l}`);
      lines.push('}');
      s = s.slice(bodyEnd + 1);
      continue;
    }
    if (s.includes('=')) {
      const end = s.indexOf(';');
      const stmt = s.slice(0, end === -1 ? undefined : end).trim();
      const [lhs, rhsRaw] = stmt.split('=', 2);
      let rhs = rhsRaw.trim().replace(/\n/g, ' ');
      if (rhs.includes('.filter(') && rhs.includes('.map(')) {
        const out = parseFilterMap(rhs, lhs.trim());
        if (out.length) for (const l of out) lines.push(l);
      } else {
        if (uninit[lhs.trim()]) {
          lines.push(`var ${lhs.trim()} = ${rhs}`);
          delete uninit[lhs.trim()];
        } else {
          lines.push(`${lhs.trim()} = ${rhs}`);
        }
      }
      s = end === -1 ? '' : s.slice(end + 1);
      continue;
    }
    const semi = s.indexOf(';');
    if (semi !== -1) s = s.slice(semi + 1);
    else s = '';
  }
  return lines;
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

function parseFilterMap(expr: string, lhs: string): string[] {
  const filterIdx = expr.indexOf('.filter(');
  const mapIdx = expr.indexOf('.map(');
  if (filterIdx === -1 || mapIdx === -1 || mapIdx < filterIdx) return [];
  const list = expr.slice(0, filterIdx).trim();
  let fStart = filterIdx + 8;
  const fEnd = findMatch(expr, fStart - 1, '(', ')');
  const fPart = expr.slice(fStart, fEnd);
  const arrow = fPart.indexOf('=>');
  if (arrow === -1) return [];
  const iter = fPart.slice(0, arrow).replace(/^[()\s]+|[()\s]+$/g, '');
  const cond = fPart.slice(arrow + 2).replace(/^[()\s]+|[()\s]+$/g, '');
  fStart = mapIdx + 5;
  const mEnd = findMatch(expr, fStart - 1, '(', ')');
  const mPart = expr.slice(fStart, mEnd);
  const arrow2 = mPart.indexOf('=>');
  if (arrow2 === -1) return [];
  const iter2 = mPart.slice(0, arrow2).replace(/^[()\s]+|[()\s]+$/g, '') || iter;
  const body = mPart.slice(arrow2 + 2).replace(/^[()\s]+|[()\s]+$/g, '');
  return [
    `${lhs} = from ${iter2} in ${list}`,
    `             where ${cond}`,
    `             select ${body}`,
  ];
}

export default { convert };
