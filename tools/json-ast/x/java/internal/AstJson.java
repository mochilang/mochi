package internal;

import com.sun.source.tree.*;
import com.sun.source.tree.LambdaExpressionTree;
import com.sun.source.util.JavacTask;
import javax.tools.*;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class AstJson {
    private final PrintWriter out;
    public AstJson(PrintWriter out) { this.out = out; }

    public static void main(String[] args) throws Exception {
        if (args.length != 1) {
            System.err.println("usage: AstJson <file>");
            System.exit(1);
        }
        File src = new File(args[0]);
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        StandardJavaFileManager fm = compiler.getStandardFileManager(null, null, null);
        Iterable<? extends JavaFileObject> fobjs = fm.getJavaFileObjects(src);
        JavacTask task = (JavacTask) compiler.getTask(null, fm, null, null, null, fobjs);
        Iterable<? extends CompilationUnitTree> units = task.parse();
        PrintWriter pw = new PrintWriter(new OutputStreamWriter(System.out, StandardCharsets.UTF_8));
        AstJson printer = new AstJson(pw);
        pw.print("{\"body\":");
        printer.printUnits(units);
        pw.print("}");
        pw.flush();
    }

    private void printUnits(Iterable<? extends CompilationUnitTree> units) {
        boolean firstStmt = true;
        out.print("[");
        for (CompilationUnitTree cu : units) {
            for (Tree t : cu.getTypeDecls()) {
                if (t.getKind() != Tree.Kind.CLASS) continue;
                ClassTree ct = (ClassTree) t;
                for (Tree m : ct.getMembers()) {
                    if (m.getKind() == Tree.Kind.VARIABLE) {
                        VariableTree vt = (VariableTree) m;
                        if (!vt.getModifiers().getFlags().contains(javax.lang.model.element.Modifier.STATIC)) continue;
                        if (!firstStmt) out.print(',');
                        firstStmt = false;
                        printVarDecl(vt);
                    }
                }
                for (Tree m : ct.getMembers()) {
                    if (m.getKind() == Tree.Kind.METHOD) {
                        MethodTree mt = (MethodTree) m;
                        if (!mt.getModifiers().getFlags().contains(javax.lang.model.element.Modifier.STATIC)) continue;
                        if (mt.getName().contentEquals("main")) continue;
                        if (!firstStmt) out.print(',');
                        firstStmt = false;
                        printFnDecl(mt);
                    }
                }
                for (Tree m : ct.getMembers()) {
                    if (m.getKind() == Tree.Kind.METHOD) {
                        MethodTree mt = (MethodTree) m;
                        if (mt.getName().contentEquals("main")) {
                            for (StatementTree st : mt.getBody().getStatements()) {
                                if (!firstStmt) out.print(',');
                                firstStmt = false;
                                printStmt(st);
                            }
                        }
                    }
                }
            }
        }
        out.print("]");
    }

    private void printVarDecl(VariableTree vt) {
        out.print("{\"kind\":\"VarDecl\",\"name\":\"");
        out.print(vt.getName());
        out.print("\"");
        if (vt.getType() != null) {
            out.print(",\"type\":\"");
            out.print(vt.getType());
            out.print("\"");
        }
        if (vt.getInitializer() != null) {
            out.print(",\"expr\":");
            printExpr(vt.getInitializer());
        }
        out.print("}");
    }

    private void printFnDecl(MethodTree mt) {
        out.print("{\"kind\":\"FnDecl\",\"name\":\"");
        out.print(mt.getName());
        out.print("\",\"params\":[");
        boolean first = true;
        for (VariableTree p : mt.getParameters()) {
            if (!first) out.print(',');
            first = false;
            out.print("{\"name\":\"");
            out.print(p.getName());
            out.print("\"");
            if (p.getType() != null) {
                out.print(",\"type\":\"");
                out.print(p.getType());
                out.print("\"");
            }
            out.print("}");
        }
        out.print("],");
        if (mt.getReturnType() != null) {
            out.print("\"type\":\"");
            out.print(mt.getReturnType());
            out.print("\",");
        }
        out.print("\"body\":");
        printBlockStatements(mt.getBody());
        out.print("}");
    }

    private void printStmt(StatementTree st) {
        switch (st.getKind()) {
        case EXPRESSION_STATEMENT:
            ExpressionStatementTree est = (ExpressionStatementTree) st;
            ExpressionTree e = est.getExpression();
            if (e.getKind() == Tree.Kind.ASSIGNMENT) {
                AssignmentTree at = (AssignmentTree) e;
                out.print("{\"kind\":\"Assign\",");
                out.print("\"name\":\"");
                out.print(at.getVariable());
                out.print("\",\"target\":");
                printExpr(at.getVariable());
                out.print(",\"expr\":");
                printExpr(at.getExpression());
                out.print("}");
                break;
            }
            if (e.getKind() == Tree.Kind.METHOD_INVOCATION) {
                MethodInvocationTree mit = (MethodInvocationTree) e;
                if (mit.getMethodSelect().getKind() == Tree.Kind.MEMBER_SELECT) {
                    MemberSelectTree ms = (MemberSelectTree) mit.getMethodSelect();
                    if ("System.out.println".equals(ms.getExpression().toString()+"."+ms.getIdentifier())) {
                        out.print("{\"kind\":\"Print\",\"expr\":");
                        if (!mit.getArguments().isEmpty()) {
                            printExpr(mit.getArguments().get(0));
                        } else {
                            out.print("{\"kind\":\"Literal\",\"value\":\"\"}");
                        }
                        out.print("}");
                        break;
                    }
                }
            }
            out.print("{\"kind\":\"Expr\"}");
            break;
        case VARIABLE:
            printVarDecl((VariableTree) st);
            break;
        case ENHANCED_FOR_LOOP:
            EnhancedForLoopTree ef = (EnhancedForLoopTree) st;
            out.print("{\"kind\":\"ForEach\",\"name\":\"");
            out.print(ef.getVariable().getName());
            out.print("\",\"expr\":");
            printExpr(ef.getExpression());
            out.print(",\"body\":");
            printBlockStatements(ef.getStatement());
            out.print("}");
            break;
        case FOR_LOOP:
            ForLoopTree fl = (ForLoopTree) st;
            if (fl.getInitializer().size() == 1 && fl.getInitializer().get(0).getKind() == Tree.Kind.VARIABLE) {
                VariableTree iv = (VariableTree) fl.getInitializer().get(0);
                out.print("{\"kind\":\"ForRange\",\"name\":\"");
                out.print(iv.getName());
                out.print("\",\"start\":");
                printExpr(iv.getInitializer());
                out.print(",\"end\":");
                if (fl.getCondition() instanceof BinaryTree) {
                    BinaryTree bt = (BinaryTree) fl.getCondition();
                    printExpr(bt.getRightOperand());
                } else {
                    printExpr(fl.getCondition());
                }
                out.print(",\"body\":");
                printBlockStatements(fl.getStatement());
                out.print("}");
                break;
            }
            out.print("{\"kind\":\"ForLoop\"}");
            break;
        case WHILE_LOOP:
            WhileLoopTree wl = (WhileLoopTree) st;
            out.print("{\"kind\":\"While\",\"cond\":");
            printExpr(wl.getCondition());
            out.print(",\"body\":");
            printBlockStatements(wl.getStatement());
            out.print("}");
            break;
        case BREAK:
            out.print("{\"kind\":\"Break\"}");
            break;
        case CONTINUE:
            out.print("{\"kind\":\"Continue\"}");
            break;
        case IF:
            IfTree it = (IfTree) st;
            out.print("{\"kind\":\"If\",\"cond\":");
            printExpr(it.getCondition());
            out.print(",\"then\":");
            printBlockStatements(it.getThenStatement());
            if (it.getElseStatement() != null) {
                out.print(",\"else\":");
                printBlockStatements(it.getElseStatement());
            }
            out.print("}");
            break;
        case RETURN:
            ReturnTree rt = (ReturnTree) st;
            out.print("{\"kind\":\"Return\"");
            if (rt.getExpression() != null) {
                out.print(",\"expr\":");
                printExpr(rt.getExpression());
            }
            out.print("}");
            break;
        default:
            out.print("{\"kind\":\"");
            out.print(st.getKind().name());
            out.print("\"}");
        }
    }

    private void printBlockStatements(StatementTree stmt) {
        if (stmt.getKind() == Tree.Kind.BLOCK) {
            BlockTree block = (BlockTree) stmt;
            out.print("[");
            boolean first = true;
            for (StatementTree s : block.getStatements()) {
                if (!first) out.print(',');
                first = false;
                printStmt(s);
            }
            out.print("]");
        } else {
            out.print("[");
            printStmt(stmt);
            out.print("]");
        }
    }

    private void printExpr(Tree expr) {
        switch (expr.getKind()) {
        case STRING_LITERAL:
            out.print("{\"kind\":\"String\",\"value\":");
            quote(((LiteralTree)expr).getValue().toString());
            out.print("}");
            break;
        case INT_LITERAL:
        case LONG_LITERAL:
        case FLOAT_LITERAL:
        case DOUBLE_LITERAL:
        case BOOLEAN_LITERAL:
            out.print("{\"kind\":\"Literal\",\"value\":\"");
            out.print(((LiteralTree)expr).getValue());
            out.print("\"}");
            break;
        case PARENTHESIZED:
            ParenthesizedTree pt = (ParenthesizedTree) expr;
            printExpr(pt.getExpression());
            break;
        case IDENTIFIER:
            out.print("{\"kind\":\"Ident\",\"name\":\"");
            out.print(((IdentifierTree)expr).getName());
            out.print("\"}");
            break;
        case MEMBER_SELECT:
            MemberSelectTree ms = (MemberSelectTree) expr;
            out.print("{\"kind\":\"Member\",\"expr\":");
            printExpr(ms.getExpression());
            out.print(",\"name\":\"");
            out.print(ms.getIdentifier());
            out.print("\"}");
            break;
        case METHOD_INVOCATION:
            MethodInvocationTree mit = (MethodInvocationTree) expr;
            out.print("{\"kind\":\"Call\",\"target\":");
            printExpr(mit.getMethodSelect());
            out.print(",\"args\":[");
            boolean f = true;
            for (ExpressionTree a : mit.getArguments()) {
                if (!f) out.print(',');
                f = false;
                printExpr(a);
            }
            out.print("]}");
            break;
        case LOGICAL_COMPLEMENT:
            UnaryTree ut = (UnaryTree) expr;
            out.print("{\"kind\":\"Unary\",\"op\":\"LOGICAL_COMPLEMENT\",\"expr\":");
            printExpr(ut.getExpression());
            out.print("}");
            break;
        case TYPE_CAST:
            TypeCastTree tct = (TypeCastTree) expr;
            out.print("{\"kind\":\"Cast\",\"value\":\"");
            out.print(tct.getType());
            out.print("\",\"expr\":");
            printExpr(tct.getExpression());
            out.print("}");
            break;
        case PLUS:
        case MINUS:
        case MULTIPLY:
        case DIVIDE:
        case REMAINDER:
        case LESS_THAN:
        case GREATER_THAN:
        case EQUAL_TO:
        case NOT_EQUAL_TO:
        case LESS_THAN_EQUAL:
        case GREATER_THAN_EQUAL:
        case CONDITIONAL_AND:
        case CONDITIONAL_OR:
            BinaryTree bt = (BinaryTree) expr;
            out.print("{\"kind\":\"Binary\",\"op\":\"");
            out.print(expr.getKind().name());
            out.print("\",\"left\":");
            printExpr(bt.getLeftOperand());
            out.print(",\"right\":");
            printExpr(bt.getRightOperand());
            out.print("}");
            break;
        case CONDITIONAL_EXPRESSION:
            ConditionalExpressionTree ct = (ConditionalExpressionTree) expr;
            out.print("{\"kind\":\"Cond\",\"cond\":");
            printExpr(ct.getCondition());
            out.print(",\"then\":");
            printExpr(ct.getTrueExpression());
            out.print(",\"else\":");
            printExpr(ct.getFalseExpression());
            out.print("}");
            break;
        case ARRAY_ACCESS:
            ArrayAccessTree aa = (ArrayAccessTree) expr;
            out.print("{\"kind\":\"Index\",\"expr\":");
            printExpr(aa.getExpression());
            out.print(",\"index\":");
            printExpr(aa.getIndex());
            out.print("}");
            break;
        case NEW_ARRAY: {
            NewArrayTree na = (NewArrayTree) expr;
            out.print("{\"kind\":\"NEW_ARRAY\"");
            if (na.getType() != null) {
                out.print(",\"type\":\"");
                out.print(na.getType());
                out.print("\"");
            }
            if (!na.getDimensions().isEmpty()) {
                out.print(",\"dims\":[");
                boolean fd = true;
                for (ExpressionTree d : na.getDimensions()) {
                    if (!fd) out.print(',');
                    fd = false;
                    printExpr(d);
                }
                out.print("]");
            }
            if (!na.getInitializers().isEmpty()) {
                out.print(",\"elems\":[");
                boolean fe = true;
                for (ExpressionTree e : na.getInitializers()) {
                    if (!fe) out.print(',');
                    fe = false;
                    printExpr(e);
                }
                out.print("]");
            }
            out.print("}");
            break;
        }
        case NEW_CLASS: {
            NewClassTree nc = (NewClassTree) expr;
            out.print("{\"kind\":\"NEW_CLASS\"");
            if (nc.getIdentifier() != null) {
                out.print(",\"type\":\"");
                out.print(nc.getIdentifier());
                out.print("\"");
            }
            out.print(",\"args\":[");
            boolean fa = true;
            for (ExpressionTree a : nc.getArguments()) {
                if (!fa) out.print(',');
                fa = false;
                printExpr(a);
            }
            out.print("]");
            ClassTree body = nc.getClassBody();
            if (body != null) {
                out.print(",\"body\":[");
                boolean fb = true;
                for (Tree m : body.getMembers()) {
                    if (m.getKind() == Tree.Kind.BLOCK) {
                        BlockTree blk = (BlockTree) m;
                        for (StatementTree st : blk.getStatements()) {
                            if (!fb) out.print(',');
                            fb = false;
                            printStmt(st);
                        }
                    } else if (m.getKind() == Tree.Kind.VARIABLE) {
                        if (!fb) out.print(',');
                        fb = false;
                        printVarDecl((VariableTree) m);
                    } else if (m.getKind() == Tree.Kind.METHOD) {
                        if (!fb) out.print(',');
                        fb = false;
                        printFnDecl((MethodTree) m);
                    }
                }
                out.print("]");
            }
            out.print("}");
            break;
        }
        case LAMBDA_EXPRESSION:
            LambdaExpressionTree lt = (LambdaExpressionTree) expr;
            out.print("{\"kind\":\"Lambda\",\"params\":[");
            boolean fp = true;
            for (VariableTree p : lt.getParameters()) {
                if (!fp) out.print(',');
                fp = false;
                out.print("{\"name\":\"");
                out.print(p.getName());
                out.print("\"");
                if (p.getType() != null) {
                    out.print(",\"type\":\"");
                    out.print(p.getType());
                    out.print("\"");
                }
                out.print("}");
            }
            out.print("],");
            if (lt.getBodyKind() == LambdaExpressionTree.BodyKind.EXPRESSION) {
                out.print("\"expr\":");
                printExpr((ExpressionTree) lt.getBody());
            } else {
                out.print("\"body\":");
                printBlockStatements((StatementTree) lt.getBody());
            }
            out.print("}");
            break;
        default:
            out.print("{\"kind\":\"");
            out.print(expr.getKind().name());
            out.print("\"}");
        }
    }

    private void quote(String s) {
        out.print('"');
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            switch (c) {
            case '\\': out.print("\\\\"); break;
            case '"': out.print("\\\""); break;
            case '\n': out.print("\\n"); break;
            case '\r': out.print("\\r"); break;
            case '\t': out.print("\\t"); break;
            default: out.print(c);
            }
        }
        out.print('"');
    }
}
