using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.Json;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

public class Field
{
    public string Name { get; set; }
    public string Type { get; set; }
    public string Access { get; set; }
    public int Line { get; set; }
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
        BaseFieldDeclarationSyntax bf = m as BaseFieldDeclarationSyntax;
        if (bf == null) return null;
        var name = "";
        TypeSyntax typ = null;
        if (bf is FieldDeclarationSyntax fd)
        {
            name = fd.Declaration.Variables.First().Identifier.Text;
            typ = fd.Declaration.Type;
        }
        else if (bf is PropertyDeclarationSyntax pd)
        {
            name = pd.Identifier.Text;
            typ = pd.Type;
        }
        var span = bf.GetLocation().GetLineSpan();
        return new Field
        {
            Name = name,
            Type = typ?.ToString() ?? "",
            Access = GetAccess(bf.Modifiers),
            Line = span.StartLinePosition.Line + 1,
            Doc = GetDoc(bf)
        };
    }

    static Method ParseMethod(MethodDeclarationSyntax m)
    {
        var span = m.GetLocation().GetLineSpan();
        var lines = m.Body?.ToFullString().Split('\n').ToList() ?? new List<string>();
        if (lines.Count > 0) lines.RemoveAt(0);
        if (lines.Count > 0) lines.RemoveAt(lines.Count - 1);
        for (int i=0;i<lines.Count;i++) lines[i] = lines[i].TrimEnd('\r');
        return new Method
        {
            Name = m.Identifier.Text,
            Params = m.ParameterList.Parameters.Select(p => new Param { Name = p.Identifier.Text, Type = p.Type?.ToString() ?? "" }).ToList(),
            Ret = m.ReturnType.ToString(),
            Access = GetAccess(m.Modifiers),
            Static = m.Modifiers.Any(SyntaxKind.StaticKeyword),
            Body = lines,
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
