public class Main {

    static void createFile(java.util.Map<String,Boolean> fs, String fn) {
        if (fs.containsKey(fn)) {
            System.out.println("open " + fn + ": file exists");
        } else {
fs.put(fn, false);
            System.out.println("file " + fn + " created!");
        }
    }

    static void createDir(java.util.Map<String,Boolean> fs, String dn) {
        if (fs.containsKey(dn)) {
            System.out.println("mkdir " + dn + ": file exists");
        } else {
fs.put(dn, true);
            System.out.println("directory " + dn + " created!");
        }
    }

    static void main() {
        java.util.Map<String,Boolean> fs = new java.util.LinkedHashMap<String, Boolean>();
fs.put("docs", true);
        createFile(fs, "input.txt");
        createFile(fs, "/input.txt");
        createDir(fs, "docs");
        createDir(fs, "/docs");
    }
    public static void main(String[] args) {
        main();
    }
}
