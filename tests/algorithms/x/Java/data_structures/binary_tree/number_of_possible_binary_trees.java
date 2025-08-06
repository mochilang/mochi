public class Main {
    static String input_str;
    static int node_count;

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static int binomial_coefficient(int n, int k) {
        int result = 1;
        int kk = k;
        if (k > n - k) {
            kk = n - k;
        }
        for (int i = 0; i < kk; i++) {
            result = result * (n - i);
            result = result / (i + 1);
        }
        return result;
    }

    static int catalan_number(int node_count) {
        return binomial_coefficient(2 * node_count, node_count) / (node_count + 1);
    }

    static int factorial(int n) {
        if (n < 0) {
            System.out.println("factorial() not defined for negative values");
            return 0;
        }
        int result_1 = 1;
        for (int i = 1; i < (n + 1); i++) {
            result_1 = result_1 * i;
        }
        return result_1;
    }

    static int binary_tree_count(int node_count) {
        return catalan_number(node_count) * factorial(node_count);
    }
    public static void main(String[] args) {
        System.out.println("Enter the number of nodes:");
        input_str = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
        node_count = Integer.parseInt(input_str);
        if (node_count <= 0) {
            System.out.println("We need some nodes to work with.");
        } else {
            int bst = catalan_number(node_count);
            int bt = binary_tree_count(node_count);
            System.out.println("Given" + " " + String.valueOf(node_count) + " " + "nodes, there are" + " " + String.valueOf(bt) + " " + "binary trees and" + " " + String.valueOf(bst) + " " + "binary search trees.");
        }
    }
}
