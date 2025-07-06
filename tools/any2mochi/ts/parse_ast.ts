import { Node, Project } from "npm:ts-morph";

interface TSParam {
  name: string;
  typ: string;
}
interface TSField {
  name: string;
  typ: string;
}
interface TSDecl {
  kind: string;
  name: string;
  params?: TSParam[];
  ret?: string;
  body?: string;
  fields?: TSField[];
  alias?: string;
  variants?: string[];
  start?: number;
  end?: number;
}

function tsToMochiType(t: string): string {
  t = t.trim();
  if (t.includes("|")) {
    const parts = t.split("|").map((p) => p.trim()).filter((p) =>
      p !== "null" && p !== "undefined"
    ).map(tsToMochiType).filter(Boolean);
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

function parse(src: string): TSDecl[] {
  const project = new Project({ useInMemoryFileSystem: true });
  const file = project.createSourceFile("input.ts", src);
  const decls: TSDecl[] = [];

  for (const stmt of file.getStatements()) {
    const start = stmt.getStartLineNumber();
    const end = stmt.getEndLineNumber();
    if (Node.isVariableStatement(stmt)) {
      for (const d of stmt.getDeclarationList().getDeclarations()) {
        const name = d.getName();
        const typ = tsToMochiType(d.getTypeNode()?.getText() || "");
        decls.push({
          kind: "var",
          name,
          start,
          end,
          fields: undefined,
          params: undefined,
          ret: undefined,
          body: undefined,
          alias: typ ? undefined : undefined,
          variants: undefined,
          ...(typ && { ret: typ }),
        });
        decls[decls.length - 1].ret = typ; // store type in ret field
      }
    } else if (Node.isFunctionDeclaration(stmt)) {
      const name = stmt.getName() || "";
      const params: TSParam[] = [];
      stmt.getParameters().forEach((p) => {
        params.push({
          name: p.getName(),
          typ: tsToMochiType(p.getTypeNode()?.getText() || ""),
        });
      });
      const rt = tsToMochiType(stmt.getReturnTypeNode()?.getText() || "");
      decls.push({
        kind: "func",
        name,
        start,
        end,
        params,
        ret: rt,
        body: stmt.getBodyText() || "",
      });
    } else if (Node.isEnumDeclaration(stmt)) {
      const variants = stmt.getMembers().map((m) => m.getName());
      decls.push({ kind: "enum", name: stmt.getName(), start, end, variants });
    } else if (
      Node.isClassDeclaration(stmt) || Node.isInterfaceDeclaration(stmt)
    ) {
      const fields: TSField[] = [];
      stmt.getMembers().forEach((mem) => {
        if (Node.isPropertyDeclaration(mem) || Node.isPropertySignature(mem)) {
          fields.push({
            name: mem.getName(),
            typ: tsToMochiType(mem.getTypeNode()?.getText() || ""),
          });
        }
      });
      decls.push({
        kind: "type",
        name: stmt.getName() || "",
        start,
        end,
        fields,
      });
    } else if (Node.isTypeAliasDeclaration(stmt)) {
      const name = stmt.getName();
      const tn = stmt.getTypeNode();
      if (tn && Node.isTypeLiteral(tn)) {
        const fields: TSField[] = [];
        tn.getMembers().forEach((m) => {
          if (Node.isPropertySignature(m)) {
            fields.push({
              name: m.getName(),
              typ: tsToMochiType(m.getTypeNode()?.getText() || ""),
            });
          }
        });
        decls.push({ kind: "type", name, start, end, fields });
      } else if (tn) {
        const alias = tsToMochiType(tn.getText());
        decls.push({ kind: "alias", name, start, end, alias });
      }
    }
  }

  return decls;
}

if (import.meta.main) {
  const file = Deno.args[0];
  const src = await Deno.readTextFile(file);
  console.log(JSON.stringify(parse(src)));
}

export default { parse };
