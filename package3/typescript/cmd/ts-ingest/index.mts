#!/usr/bin/env node
// ts-ingest: extracts ApiSurface JSON from TypeScript .d.ts files.
// Usage: node index.mjs <dts-dir> --out <output.json>
//
// MEP-72 Phase 3.

import * as ts from "typescript";
import * as fs from "fs";
import * as path from "path";

interface Param {
  name: string;
  type: string;
  optional?: boolean;
}

interface Export {
  kind: "function" | "class" | "interface" | "type" | "const" | "enum";
  name: string;
  params?: Param[];
  returnType?: string;
  typeParams?: string[];
  isAsync?: boolean;
  deprecated?: boolean;
  internal?: boolean;
  browserOnly?: boolean;
}

interface ApiSurface {
  packageName: string;
  version: string;
  exports: Record<string, Export[]>;
}

function hasJSDocTag(node: ts.Node, tag: string): boolean {
  const jsDocs = ts.getJSDocTags(node);
  return jsDocs.some((t) => t.tagName.text === tag);
}

function isBrowserOnly(node: ts.Node): boolean {
  const jsDocs = ts.getJSDocTags(node);
  for (const t of jsDocs) {
    const text = t.comment?.toString() ?? "";
    if (t.tagName.text === "remarks" && text.includes("DOM")) return true;
    if (t.tagName.text === "environment" && text.includes("browser"))
      return true;
  }
  return false;
}

function typeToString(
  type: ts.TypeNode | undefined,
  checker: ts.TypeChecker
): string {
  if (!type) return "void";
  return type.getText();
}

function extractExports(
  sourceFile: ts.SourceFile,
  checker: ts.TypeChecker
): Export[] {
  const result: Export[] = [];

  function visit(node: ts.Node) {
    if (!ts.isModuleBlock(node) && !ts.isSourceFile(node)) {
      if (ts.isModuleDeclaration(node)) {
        ts.forEachChild(node, visit);
        return;
      }
    }

    if (ts.isFunctionDeclaration(node) && node.name) {
      const isExported = node.modifiers?.some(
        (m) => m.kind === ts.SyntaxKind.ExportKeyword
      );
      if (!isExported) {
        ts.forEachChild(node, visit);
        return;
      }
      const exp: Export = {
        kind: "function",
        name: node.name.text,
        params: node.parameters.map((p) => ({
          name: p.name.getText(),
          type: typeToString(p.type, checker),
          optional: !!p.questionToken,
        })),
        returnType: typeToString(node.type, checker),
        typeParams: node.typeParameters?.map((tp) => tp.name.text),
        isAsync: node.modifiers?.some(
          (m) => m.kind === ts.SyntaxKind.AsyncKeyword
        ),
        deprecated: hasJSDocTag(node, "deprecated"),
        internal: hasJSDocTag(node, "internal"),
        browserOnly: isBrowserOnly(node),
      };
      result.push(exp);
    } else if (
      ts.isVariableStatement(node) &&
      node.modifiers?.some((m) => m.kind === ts.SyntaxKind.ExportKeyword)
    ) {
      for (const decl of node.declarationList.declarations) {
        if (ts.isIdentifier(decl.name)) {
          result.push({
            kind: "const",
            name: decl.name.text,
            returnType: typeToString(decl.type, checker),
            deprecated: hasJSDocTag(node, "deprecated"),
            internal: hasJSDocTag(node, "internal"),
          });
        }
      }
    } else if (ts.isClassDeclaration(node) && node.name) {
      const isExported = node.modifiers?.some(
        (m) => m.kind === ts.SyntaxKind.ExportKeyword
      );
      if (isExported) {
        result.push({
          kind: "class",
          name: node.name.text,
          typeParams: node.typeParameters?.map((tp) => tp.name.text),
          deprecated: hasJSDocTag(node, "deprecated"),
          internal: hasJSDocTag(node, "internal"),
        });
      }
    } else if (ts.isInterfaceDeclaration(node)) {
      const isExported = node.modifiers?.some(
        (m) => m.kind === ts.SyntaxKind.ExportKeyword
      );
      if (isExported) {
        result.push({
          kind: "interface",
          name: node.name.text,
          typeParams: node.typeParameters?.map((tp) => tp.name.text),
        });
      }
    } else if (ts.isEnumDeclaration(node)) {
      const isExported = node.modifiers?.some(
        (m) => m.kind === ts.SyntaxKind.ExportKeyword
      );
      if (isExported) {
        result.push({ kind: "enum", name: node.name.text });
      }
    }

    ts.forEachChild(node, visit);
  }

  ts.forEachChild(sourceFile, visit);
  return result;
}

function main() {
  const args = process.argv.slice(2);
  let dtsDir = "";
  let outFile = "";
  let pkgName = "";
  let pkgVersion = "0.0.0";

  for (let i = 0; i < args.length; i++) {
    if (args[i] === "--out" && args[i + 1]) {
      outFile = args[++i];
    } else if (args[i] === "--package" && args[i + 1]) {
      pkgName = args[++i];
    } else if (args[i] === "--version" && args[i + 1]) {
      pkgVersion = args[++i];
    } else if (!dtsDir) {
      dtsDir = args[i];
    }
  }

  if (!dtsDir || !outFile) {
    console.error("Usage: ts-ingest <dts-dir> --out <output.json> [--package <name>] [--version <ver>]");
    process.exit(1);
  }

  // Try to read package.json for name/version if not provided.
  const pkgJsonPath = path.join(dtsDir, "package.json");
  if (fs.existsSync(pkgJsonPath)) {
    try {
      const pkg = JSON.parse(fs.readFileSync(pkgJsonPath, "utf8"));
      if (!pkgName) pkgName = pkg.name ?? path.basename(dtsDir);
      if (pkgVersion === "0.0.0") pkgVersion = pkg.version ?? "0.0.0";
    } catch {}
  }
  if (!pkgName) pkgName = path.basename(dtsDir);

  // Find all .d.ts files.
  const dtsFiles: string[] = [];
  function walkDir(dir: string) {
    for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
      const full = path.join(dir, entry.name);
      if (entry.isDirectory() && entry.name !== "node_modules") {
        walkDir(full);
      } else if (entry.isFile() && entry.name.endsWith(".d.ts")) {
        dtsFiles.push(full);
      }
    }
  }
  walkDir(dtsDir);

  if (dtsFiles.length === 0) {
    console.error("ts-ingest: no .d.ts files found in", dtsDir);
    process.exit(1);
  }

  const program = ts.createProgram(dtsFiles, {
    target: ts.ScriptTarget.ES2022,
    module: ts.ModuleKind.ESNext,
    strict: false,
  });
  const checker = program.getTypeChecker();

  const surface: ApiSurface = {
    packageName: pkgName,
    version: pkgVersion,
    exports: {},
  };

  for (const sf of program.getSourceFiles()) {
    if (!sf.isDeclarationFile) continue;
    const rel = path.relative(dtsDir, sf.fileName);
    if (rel.startsWith("..")) continue;
    const modPath = "./" + rel.replace(/\.d\.ts$/, "");
    const exports = extractExports(sf, checker);
    if (exports.length > 0) {
      surface.exports[modPath] = exports;
    }
  }

  fs.writeFileSync(outFile, JSON.stringify(surface, null, 2));
  console.log(`ts-ingest: wrote ${outFile} (${Object.values(surface.exports).flat().length} exports)`);
}

main();
