using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.Json;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

public class Node
{
    public string Kind { get; set; }
    public string Value { get; set; }
    public List<Node> Children { get; set; } = new List<Node>();
}

public class Field
{
    public string Name { get; set; }
    public string Type { get; set; }
    public string Access { get; set; }
    public int Line { get; set; }
    public string Value { get; set; }
    public Node Ast { get; set; }
    public bool Static { get; set; }
    public string Doc { get; set; }
}

public class Param
{
    public string Name { get; set; }
    public string Type { get; set; }
}

public class Method
{
    public string Name { get; set; }
    public List<Param> Params { get; set; } = new List<Param>();
    public string Ret { get; set; }
    public string Access { get; set; }
    public bool Static { get; set; }
    public List<string> Body { get; set; } = new List<string>();
    public Node Ast { get; set; }
    public int StartLine { get; set; }
    public int EndLine { get; set; }
    public string Doc { get; set; }
}

public class TypeDecl
{
    public string Name { get; set; }
    public string Kind { get; set; }
    public string Access { get; set; }
    public int StartLine { get; set; }
    public int EndLine { get; set; }
    public List<Field> Fields { get; set; } = new List<Field>();
    public List<Method> Methods { get; set; } = new List<Method>();
    public string Doc { get; set; }
}

public class AST
{
    public List<TypeDecl> Types { get; set; } = new List<TypeDecl>();
    public string Src { get; set; }
}

public static class AstJson
{
    static string GetAccess(SyntaxTokenList mods)
    {
        if (mods.Any(SyntaxKind.PublicKeyword)) return "public";
        if (mods.Any(SyntaxKind.ProtectedKeyword)) return "protected";
        if (mods.Any(SyntaxKind.PrivateKeyword)) return "private";
        return "";
    }

    static string GetDoc(SyntaxNode node)
    {
        var lines = new List<string>();
        foreach (var t in node.GetLeadingTrivia())
        {
            if (t.IsKind(SyntaxKind.SingleLineCommentTrivia))
            {
                var s = t.ToString().TrimStart('/').Trim();
                lines.Add(s);
            }
        }
        return string.Join("\n", lines);
    }

    static Field ParseField(MemberDeclarationSyntax m)
    {
        var name = "";
        TypeSyntax typ = null;
        SyntaxTokenList modifiers = default;
        FileLinePositionSpan span = default;
        bool isStatic = false;
        string value = "";
        Node ast = null;

        if (m is FieldDeclarationSyntax fd)
        {
            name = fd.Declaration.Variables.First().Identifier.Text;
            typ = fd.Declaration.Type;
            modifiers = fd.Modifiers;
            span = fd.GetLocation().GetLineSpan();
            var v = fd.Declaration.Variables.First();
            if (v.Initializer != null)
            {
                value = v.Initializer.Value.ToString();
                ast = ParseExpr(v.Initializer.Value);
            }
            isStatic = fd.Modifiers.Any(SyntaxKind.StaticKeyword);
        }
        else if (m is PropertyDeclarationSyntax pd)
        {
            name = pd.Identifier.Text;
            typ = pd.Type;
            modifiers = pd.Modifiers;
            span = pd.GetLocation().GetLineSpan();
            isStatic = pd.Modifiers.Any(SyntaxKind.StaticKeyword);
            if (pd.Initializer != null)
            {
                value = pd.Initializer.Value.ToString();
                ast = ParseExpr(pd.Initializer.Value);
            }
        }
        else
        {
            return null;
        }

        return new Field
        {
            Name = name,
            Type = typ?.ToString() ?? "",
            Access = GetAccess(modifiers),
            Line = span.StartLinePosition.Line + 1,
            Value = value,
            Ast = ast,
            Static = isStatic,
            Doc = GetDoc(m)
        };
    }

    static Node ParseExpr(ExpressionSyntax e)
    {
        switch (e)
        {
            case CastExpressionSyntax ce:
                return ParseExpr(ce.Expression);
            case PredefinedTypeSyntax pt:
                return new Node { Kind = "ident", Value = pt.Keyword.Text };
            case LiteralExpressionSyntax lit:
                return new Node { Kind = "literal", Value = lit.Token.ValueText };
            case IdentifierNameSyntax id:
                return new Node { Kind = "ident", Value = id.Identifier.Text };
            case ElementAccessExpressionSyntax idx:
                var target = ParseExpr(idx.Expression);
                var ie = new Node { Kind = "index", Children = new List<Node> { target } };
                foreach (var arg in idx.ArgumentList.Arguments)
                    ie.Children.Add(ParseExpr(arg.Expression));
                return ie;
            case ArrayCreationExpressionSyntax arr:
                var arrNode = new Node { Kind = "array" };
                if (arr.Initializer != null)
                {
                    foreach (var ex in arr.Initializer.Expressions)
                        arrNode.Children.Add(ParseExpr(ex));
                }
                return arrNode;
            case BinaryExpressionSyntax bin:
                return new Node { Kind = "binary", Value = bin.OperatorToken.Text, Children = new List<Node> { ParseExpr(bin.Left), ParseExpr(bin.Right) } };
            case PrefixUnaryExpressionSyntax pre:
                return new Node { Kind = "unary", Value = pre.OperatorToken.Text, Children = new List<Node> { ParseExpr(pre.Operand) } };
            case ParenthesizedExpressionSyntax par:
                return ParseExpr(par.Expression);
            case InvocationExpressionSyntax inv:
                return ParseCall(inv);
            case ConditionalExpressionSyntax cond:
                return new Node { Kind = "cond", Children = new List<Node> { ParseExpr(cond.Condition), ParseExpr(cond.WhenTrue), ParseExpr(cond.WhenFalse) } };
            case MemberAccessExpressionSyntax mem:
                return new Node { Kind = "member", Value = mem.Name.Identifier.Text, Children = new List<Node> { ParseExpr(mem.Expression) } };
            case ObjectCreationExpressionSyntax obj:
                var on = new Node { Kind = "new", Value = obj.Type.ToString() };
                if (obj.Initializer != null)
                    on.Children.Add(ParseInitializer(obj.Initializer));
                return on;
            case ImplicitObjectCreationExpressionSyntax imp:
                var ion = new Node { Kind = "new", Value = "" };
                if (imp.Initializer != null)
                    ion.Children.Add(ParseInitializer(imp.Initializer));
                return ion;
        }
        return new Node { Kind = "unknown" };
    }

    static Node ParseInitializer(InitializerExpressionSyntax init)
    {
        var n = new Node { Kind = "init" };
        foreach (var expr in init.Expressions)
        {
            if (expr is AssignmentExpressionSyntax ass)
            {
                n.Children.Add(new Node { Kind = "field", Value = ass.Left.ToString(), Children = new List<Node> { ParseExpr(ass.Right) } });
            }
            else if (expr is InitializerExpressionSyntax ie && ie.Expressions.Count == 2)
            {
                n.Children.Add(new Node { Kind = "pair", Children = new List<Node> { ParseExpr(ie.Expressions[0]), ParseExpr(ie.Expressions[1]) } });
            }
            else
            {
                n.Children.Add(ParseExpr(expr));
            }
        }
        return n;
    }

    static Node ParseCall(InvocationExpressionSyntax inv)
    {
        string name = "";
        List<Node> args = new List<Node>();
        foreach (var a in inv.ArgumentList.Arguments)
            args.Add(ParseExpr(a.Expression));

        if (inv.Expression is MemberAccessExpressionSyntax ma)
        {
            if (ma.Expression.ToString() == "Console" && ma.Name.Identifier.Text == "WriteLine")
            {
                return new Node { Kind = "call", Value = "print", Children = args };
            }
            name = ma.Name.Identifier.Text;
            var target = ParseExpr(ma.Expression);
            args.Insert(0, target);
            return new Node { Kind = "call", Value = name, Children = args };
        }
        if (inv.Expression is IdentifierNameSyntax id)
        {
            name = id.Identifier.Text;
            return new Node { Kind = "call", Value = name, Children = args };
        }
        return new Node { Kind = "call", Value = inv.Expression.ToString(), Children = args };
    }

    static Node ParseStmt(StatementSyntax st)
    {
        switch (st)
        {
            case LocalDeclarationStatementSyntax ld:
                var v = ld.Declaration.Variables.First();
                var node = new Node { Kind = "var", Value = v.Identifier.Text };
                if (v.Initializer != null) node.Children.Add(ParseExpr(v.Initializer.Value));
                return node;
            case ExpressionStatementSyntax es:
                if (es.Expression is AssignmentExpressionSyntax ass)
                {
                    var asn = new Node { Kind = "assign", Value = ass.Left.ToString(), Children = new List<Node> { ParseExpr(ass.Right) } };
                    return asn;
                }
                if (es.Expression is InvocationExpressionSyntax inv)
                {
                    return ParseCall(inv);
                }
                return new Node { Kind = "expr" };
            case ForStatementSyntax fs:
                string name = "i";
                Node start = null;
                Node end = null;
                if (fs.Declaration != null && fs.Declaration.Variables.Count == 1)
                {
                    var fv = fs.Declaration.Variables.First();
                    name = fv.Identifier.Text;
                    if (fv.Initializer != null) start = ParseExpr(fv.Initializer.Value);
                }
                if (fs.Condition is BinaryExpressionSyntax be && be.Left.ToString() == name)
                    end = ParseExpr(be.Right);
                var body = ParseBlock(fs.Statement as BlockSyntax);
                var range = new Node { Kind = "range", Children = new List<Node> { start, end } };
                return new Node { Kind = "for", Value = name, Children = new List<Node> { range, body } };
            case ForEachStatementSyntax fe:
                var fbody = ParseBlock(fe.Statement as BlockSyntax ?? SyntaxFactory.Block(fe.Statement));
                var frange = new Node { Kind = "range", Children = new List<Node> { ParseExpr(fe.Expression) } };
                return new Node { Kind = "for", Value = fe.Identifier.Text, Children = new List<Node> { frange, fbody } };
            case WhileStatementSyntax ws:
                var wcond = ParseExpr(ws.Condition);
                var wbody = ParseBlock(ws.Statement as BlockSyntax);
                return new Node { Kind = "while", Children = new List<Node> { wcond, wbody } };
            case IfStatementSyntax iff:
                var cond = ParseExpr(iff.Condition);
                var thenN = ParseBlock(iff.Statement as BlockSyntax ?? SyntaxFactory.Block(iff.Statement));
                var ifn = new Node { Kind = "if", Children = new List<Node> { cond, thenN } };
                if (iff.Else != null)
                {
                    var elseN = ParseBlock(iff.Else.Statement as BlockSyntax ?? SyntaxFactory.Block(iff.Else.Statement));
                    ifn.Children.Add(elseN);
                }
                return ifn;
            case ReturnStatementSyntax ret:
                var rn = new Node { Kind = "return" };
                if (ret.Expression != null) rn.Children.Add(ParseExpr(ret.Expression));
                return rn;
            case BreakStatementSyntax:
                return new Node { Kind = "break" };
            case ContinueStatementSyntax:
                return new Node { Kind = "continue" };
            case BlockSyntax bs:
                return ParseBlock(bs);
        }
        return new Node { Kind = "unknown" };
    }

    static Node ParseBlock(BlockSyntax b)
    {
        var blk = new Node { Kind = "block" };
        if (b != null)
        {
            foreach (var st in b.Statements)
            {
                blk.Children.Add(ParseStmt(st));
            }
        }
        return blk;
    }

    static Method ParseMethod(MethodDeclarationSyntax m)
    {
        var span = m.GetLocation().GetLineSpan();
        var lines = m.Body?.ToFullString().Split('\n').ToList() ?? new List<string>();
        if (lines.Count > 0) lines.RemoveAt(0);
        if (lines.Count > 0) lines.RemoveAt(lines.Count - 1);
        if (lines.Count > 0 && lines[lines.Count - 1].Trim() == "}")
            lines.RemoveAt(lines.Count - 1);
        for (int i=0;i<lines.Count;i++) lines[i] = lines[i].TrimEnd('\r');
        return new Method
        {
            Name = m.Identifier.Text,
            Params = m.ParameterList.Parameters.Select(p => new Param { Name = p.Identifier.Text, Type = p.Type?.ToString() ?? "" }).ToList(),
            Ret = m.ReturnType.ToString(),
            Access = GetAccess(m.Modifiers),
            Static = m.Modifiers.Any(SyntaxKind.StaticKeyword),
            Body = lines,
            Ast = ParseBlock(m.Body),
            StartLine = span.StartLinePosition.Line + 1,
            EndLine = span.EndLinePosition.Line + 1,
            Doc = GetDoc(m)
        };
    }

    static TypeDecl ParseType(TypeDeclarationSyntax t)
    {
        var span = t.GetLocation().GetLineSpan();
        var td = new TypeDecl
        {
            Name = t.Identifier.Text,
            Kind = t.Kind().ToString().Replace("Declaration", "").ToLower(),
            Access = GetAccess(t.Modifiers),
            StartLine = span.StartLinePosition.Line + 1,
            EndLine = span.EndLinePosition.Line + 1,
            Doc = GetDoc(t)
        };
        foreach (var m in t.Members)
        {
            if (m is MethodDeclarationSyntax md)
                td.Methods.Add(ParseMethod(md));
            else if (m is FieldDeclarationSyntax || m is PropertyDeclarationSyntax)
            {
                var f = ParseField(m);
                if (f != null) td.Fields.Add(f);
            }
        }
        return td;
    }

    public static void Main(string[] args)
    {
        var src = File.ReadAllText(args[0]);
        var tree = CSharpSyntaxTree.ParseText(src);
        var root = tree.GetCompilationUnitRoot();
        var ast = new AST { Src = src };
        foreach (var td in root.DescendantNodes().OfType<TypeDeclarationSyntax>())
        {
            ast.Types.Add(ParseType(td));
        }
        var opt = new JsonSerializerOptions { WriteIndented = false };
        Console.Write(JsonSerializer.Serialize(ast, opt));
    }
}
