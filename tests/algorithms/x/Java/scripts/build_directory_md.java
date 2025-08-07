public class Main {
    static String[] sample;

    static String[] split(String s, String sep) {
        String[] parts = ((String[])(new String[]{}));
        String cur = "";
        int i = 0;
        while (i < _runeLen(s)) {
            if (_runeLen(sep) > 0 && i + _runeLen(sep) <= _runeLen(s) && (_substr(s, i, i + _runeLen(sep)).equals(sep))) {
                parts = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new)));
                cur = "";
                i = i + _runeLen(sep);
            } else {
                cur = cur + _substr(s, i, i + 1);
                i = i + 1;
            }
        }
        parts = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new)));
        return parts;
    }

    static String join(String[] xs, String sep) {
        String res = "";
        int i_1 = 0;
        while (i_1 < xs.length) {
            if (i_1 > 0) {
                res = res + sep;
            }
            res = res + xs[i_1];
            i_1 = i_1 + 1;
        }
        return res;
    }

    static String repeat(String s, int n) {
        String out = "";
        int i_2 = 0;
        while (i_2 < n) {
            out = out + s;
            i_2 = i_2 + 1;
        }
        return out;
    }

    static String replace_char(String s, String old, String new_) {
        String out_1 = "";
        int i_3 = 0;
        while (i_3 < _runeLen(s)) {
            String c = _substr(s, i_3, i_3 + 1);
            if ((c.equals(old))) {
                out_1 = out_1 + new_;
            } else {
                out_1 = out_1 + c;
            }
            i_3 = i_3 + 1;
        }
        return out_1;
    }

    static boolean contains(String s, String sub) {
        if (_runeLen(sub) == 0) {
            return true;
        }
        int i_4 = 0;
        while (i_4 + _runeLen(sub) <= _runeLen(s)) {
            if ((_substr(s, i_4, i_4 + _runeLen(sub)).equals(sub))) {
                return true;
            }
            i_4 = i_4 + 1;
        }
        return false;
    }

    static String file_extension(String name) {
        int i_5 = _runeLen(name) - 1;
        while (i_5 >= 0) {
            if ((_substr(name, i_5, i_5 + 1).equals("."))) {
                return name.substring(i_5, _runeLen(name));
            }
            i_5 = i_5 - 1;
        }
        return "";
    }

    static String remove_extension(String name) {
        int i_6 = _runeLen(name) - 1;
        while (i_6 >= 0) {
            if ((_substr(name, i_6, i_6 + 1).equals("."))) {
                return name.substring(0, i_6);
            }
            i_6 = i_6 - 1;
        }
        return name;
    }

    static String title_case(String s) {
        String out_2 = "";
        boolean cap = true;
        int i_7 = 0;
        while (i_7 < _runeLen(s)) {
            String c_1 = _substr(s, i_7, i_7 + 1);
            if ((c_1.equals(" "))) {
                out_2 = out_2 + c_1;
                cap = true;
            } else             if (cap) {
                out_2 = out_2 + c_1.toUpperCase();
                cap = false;
            } else {
                out_2 = out_2 + c_1.toLowerCase();
            }
            i_7 = i_7 + 1;
        }
        return out_2;
    }

    static int count_char(String s, String ch) {
        int cnt = 0;
        int i_8 = 0;
        while (i_8 < _runeLen(s)) {
            if ((_substr(s, i_8, i_8 + 1).equals(ch))) {
                cnt = cnt + 1;
            }
            i_8 = i_8 + 1;
        }
        return cnt;
    }

    static String md_prefix(int level) {
        if (level == 0) {
            return "\n##";
        }
        return (String)(_repeat("  ", level)) + "*";
    }

    static String print_path(String old_path, String new_path) {
        String[] old_parts = ((String[])(old_path.split(java.util.regex.Pattern.quote("/"))));
        String[] new_parts = ((String[])(new_path.split(java.util.regex.Pattern.quote("/"))));
        int i_9 = 0;
        while (i_9 < new_parts.length) {
            if ((i_9 >= old_parts.length || !(old_parts[i_9].equals(new_parts[i_9]))) && !(new_parts[i_9].equals(""))) {
                String title = String.valueOf(title_case(String.valueOf(replace_char(new_parts[i_9], "_", " "))));
                System.out.println(String.valueOf(md_prefix(i_9)) + " " + title);
            }
            i_9 = i_9 + 1;
        }
        return new_path;
    }

    static String[] sort_strings(String[] xs) {
        String[] arr = ((String[])(xs));
        int i_10 = 0;
        while (i_10 < arr.length) {
            int min_idx = i_10;
            int j = i_10 + 1;
            while (j < arr.length) {
                if ((arr[j].compareTo(arr[min_idx]) < 0)) {
                    min_idx = j;
                }
                j = j + 1;
            }
            String tmp = arr[i_10];
arr[i_10] = arr[min_idx];
arr[min_idx] = tmp;
            i_10 = i_10 + 1;
        }
        return arr;
    }

    static String[] good_file_paths(String[] paths) {
        String[] res_1 = ((String[])(new String[]{}));
        for (String p : paths) {
            String[] parts_1 = ((String[])(p.split(java.util.regex.Pattern.quote("/"))));
            boolean skip = false;
            int k = 0;
            while (k < parts_1.length - 1) {
                String part = parts_1[k];
                if ((part.equals("scripts")) || (part.substring(0, 1).equals(".")) || (part.substring(0, 1).equals("_")) || ((Boolean)(contains(part, "venv")))) {
                    skip = true;
                }
                k = k + 1;
            }
            if (skip) {
                continue;
            }
            String filename = parts_1[parts_1.length - 1];
            if ((filename.equals("__init__.py"))) {
                continue;
            }
            String ext = String.valueOf(file_extension(filename));
            if ((ext.equals(".py")) || (ext.equals(".ipynb"))) {
                res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(p)).toArray(String[]::new)));
            }
        }
        return res_1;
    }

    static void print_directory_md(String[] paths) {
        String[] files = ((String[])(sort_strings(((String[])(good_file_paths(((String[])(paths))))))));
        String old_path = "";
        int i_11 = 0;
        while (i_11 < files.length) {
            String fp = files[i_11];
            String[] parts_2 = ((String[])(fp.split(java.util.regex.Pattern.quote("/"))));
            String filename_1 = parts_2[parts_2.length - 1];
            String filepath = "";
            if (parts_2.length > 1) {
                filepath = String.valueOf(join(((String[])(java.util.Arrays.copyOfRange(parts_2, 0, parts_2.length - 1))), "/"));
            }
            if (!(filepath.equals(old_path))) {
                old_path = String.valueOf(print_path(old_path, filepath));
            }
            int indent = 0;
            if (_runeLen(filepath) > 0) {
                indent = count_char(filepath, "/") + 1;
            }
            String url = String.valueOf(replace_char(fp, " ", "%20"));
            String name = String.valueOf(title_case(String.valueOf(replace_char(String.valueOf(remove_extension(filename_1)), "_", " "))));
            System.out.println(String.valueOf(md_prefix(indent)) + " [" + name + "](" + url + ")");
            i_11 = i_11 + 1;
        }
    }
    public static void main(String[] args) {
        sample = ((String[])(new String[]{"data_structures/linked_list.py", "data_structures/binary_tree.py", "math/number_theory/prime_check.py", "math/number_theory/greatest_common_divisor.ipynb"}));
        print_directory_md(((String[])(sample)));
    }

    static String _repeat(String s, int n) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < n; i++) sb.append(s);
        return sb.toString();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
