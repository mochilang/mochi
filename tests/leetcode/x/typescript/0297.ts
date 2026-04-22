import * as fs from "fs";

class TreeNode {
  val: number;
  left: TreeNode | null = null;
  right: TreeNode | null = null;
  constructor(val: number) { this.val = val; }
}

function serialize(root: TreeNode | null): string {
  if (root === null) return "[]";
  const out: string[] = [];
  const q: (TreeNode | null)[] = [root];
  while (q.length > 0) {
    const node = q.shift()!;
    if (node === null) {
      out.push("null");
    } else {
      out.push(String(node.val));
      q.push(node.left, node.right);
    }
  }
  while (out.length > 0 && out[out.length - 1] === "null") out.pop();
  return `[${out.join(",")}]`;
}

function deserialize(data: string): TreeNode | null {
  if (data === "[]") return null;
  const vals = data.slice(1, -1).split(",");
  const root = new TreeNode(parseInt(vals[0], 10));
  const q: TreeNode[] = [root];
  let i = 1;
  while (q.length > 0 && i < vals.length) {
    const node = q.shift()!;
    if (i < vals.length && vals[i] !== "null") {
      node.left = new TreeNode(parseInt(vals[i], 10));
      q.push(node.left);
    }
    i++;
    if (i < vals.length && vals[i] !== "null") {
      node.right = new TreeNode(parseInt(vals[i], 10));
      q.push(node.right);
    }
    i++;
  }
  return root;
}

const lines = fs.readFileSync(0, "utf8").split(/\r?\n/).map(s => s.trim()).filter(Boolean);
if (lines.length > 0) {
  const t = parseInt(lines[0], 10);
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    out.push(serialize(deserialize(lines[tc + 1])));
  }
  process.stdout.write(out.join("\n\n"));
}
