import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * Created by bmk on 12/11/16.
 */
public class Solution {

    private static Scanner scanner = new Scanner(System.in);
    private static Node[] nodes;
    private static Edge[] edges;

    public static void main(String[] args) {
        readInput();

        System.out.println("done");
    }

    public static void readInput() {
        int n = scanner.nextInt();

        // read nodes
        nodes = new Node[n];
        for (int i = 0; i < n; i++) {
            int c = scanner.nextInt();
            nodes[i] = new Node(i, c);
        }

        // read edges
        edges = new Edge[n - 1];
        for (int i = 0; i < n - 1; i++) {
            int xId = scanner.nextInt() - 1;
            int yId = scanner.nextInt() - 1;

            edges[i] = new Edge(xId, yId);
        }

        // build tree
        for (Edge e : edges) {
            Node parent = nodes[e.xId];
            Node child = nodes[e.yId];

            child.addParent(parent);
        }
    }

    public static class Node {
        final int id;
        final int data;
        long sum;
        Node parent;

        public Node(int id, int data) {
            this.id = id;
            this.data = data;
            this.sum = data;
            this.parent = null;
        }

        public void addParent(Node parent) {
            if (this.parent != null) {
                throw new RuntimeException("Panic: parent was already set!!!");
            }

            this.parent = parent;

            while (parent != null) {
                parent.sum += sum;
                parent = parent.parent;
            }
        }
    }

    public static class Edge {
        final int xId;
        final int yId;

        public Edge(int xId, int yId) {
            this.xId = xId;
            this.yId = yId;
        }
    }
}
