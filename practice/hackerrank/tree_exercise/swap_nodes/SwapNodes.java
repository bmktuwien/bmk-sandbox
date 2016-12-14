import java.util.Scanner;

/**
 * Created by bmk on 12/8/16.
 */
public class SwapNodes {

    private static Scanner scanner = new Scanner(System.in);

    public static void main(String[] args) {
        Node tree = readTree();
        int[] ks = readKs();

        for (int k : ks) {
            swap(tree, k, 1);
            printInOrder(tree);
            System.out.println();
        }
    }

    public static void swap(Node tree, int k, int depth) {
        if (tree != null) {
            if (depth % k == 0) {
                tree.swapChildren();
            }

            swap(tree.getLeft(), k, depth + 1);
            swap(tree.getRight(), k, depth + 1);
        }
    }

    public static void printInOrder(Node n) {
        if (n != null) {
            printInOrder(n.getLeft());
            System.out.print(n.getValue() + " ");
            printInOrder(n.getRight());
        }
    }

    public static int[] readKs() {
        int n = scanner.nextInt();

        int[] ks = new int[n];
        for (int i = 0; i < ks.length; i++) {
            ks[i] = scanner.nextInt();
        }

        return ks;
    }
    public static Node readTree() {
        int n = scanner.nextInt();

        // allocate n nodes and set their values
        Node[] nodes = new Node[n];
        for (int i = 0; i < nodes.length; i++) {
            nodes[i] = new Node();
            nodes[i].setValue(i + 1);
        }

        // read tree structure
        for (Node node : nodes) {
            int leftIndex = scanner.nextInt();
            int rightIndex = scanner.nextInt();

            if (leftIndex != -1) {
                node.setLeft(nodes[leftIndex - 1]);
            }

            if (rightIndex != -1) {
                node.setRight(nodes[rightIndex - 1]);
            }
        }

        return nodes[0];
    }

    public static class Node {
        int value;
        Node left;
        Node right;

        public int getValue() {
            return value;
        }

        public void setValue(int value) {
            this.value = value;
        }

        public Node getLeft() {
            return left;
        }

        public void setLeft(Node left) {
            this.left = left;
        }

        public Node getRight() {
            return right;
        }

        public void setRight(Node right) {
            this.right = right;
        }

        public void swapChildren() {
            Node tmp = this.right;
            this.right = this.left;
            this.left = tmp;
        }
    }
}
