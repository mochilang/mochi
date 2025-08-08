public class Main {
    static String pyproject;
    static String project;

    static String parse_project_name(String toml) {
        int i = 0;
        String name = "";
        int n = _runeLen(toml);
        while (i + 4 < n) {
            if ((toml.substring(i, i+1).equals("n")) && (toml.substring(i + 1, i + 1+1).equals("a")) && (toml.substring(i + 2, i + 2+1).equals("m")) && (toml.substring(i + 3, i + 3+1).equals("e"))) {
                i = i + 4;
                while (i < n && !(toml.substring(i, i+1).equals("\""))) {
                    i = i + 1;
                }
                i = i + 1;
                while (i < n && !(toml.substring(i, i+1).equals("\""))) {
                    name = name + toml.substring(i, i+1);
                    i = i + 1;
                }
                return name;
            }
            i = i + 1;
        }
        return name;
    }
    public static void main(String[] args) {
        pyproject = "[project]\nname = \"thealgorithms-python\"";
        project = String.valueOf(parse_project_name(pyproject));
        System.out.println(project);
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
