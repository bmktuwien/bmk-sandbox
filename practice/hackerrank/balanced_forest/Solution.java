import java.util.*;

/**
 * Created by bmk on 12/11/16.
 */
public class Solution {

    private static Scanner scanner = new Scanner(System.in);
    private static Node[] nodes;
    private static Edge[] edges;

    public static void main(String[] args) {
        int q = scanner.nextInt();

        for (int i = 0; i < q; i++) {
            readQuery();
            long result = solve();
            System.out.println(result);
        }
    }

    public static long solve() {
        long result = -1;

        // brute force all edge pairs
        for (int i = 0; i < edges.length; i++) {
            for (int j = i + 1; j < edges.length; j++) {
                Edge e1 = edges[i];
                Edge e2 = edges[j];

                Node child = nodes[e1.xId].parent == nodes[e1.yId] ? nodes[e1.xId] : nodes[e1.yId];
                Node child2 = nodes[e2.xId].parent == nodes[e2.yId] ? nodes[e2.xId] : nodes[e2.yId];

                // make sure child is at bottom
                if (child2.sum < child.sum) {
                    Node tmp = child;
                    child = child2;
                    child2 = tmp;
                }

                long sum1 = child.sum;
                long sum2 = child2.sum;
                long sum3 = 0;

                Node p = child.parent;
                while (p.parent != null) {
                    if (p.parent == child2) {
                        sum2 -= sum1;
                        sum3 -= sum1;
                    }

                    p = p.parent;
                }

                sum3 += p.sum;

                // calculate minimum cw
                long min = Long.MAX_VALUE;
                if (sum1 == sum2 && sum3 < sum1 && sum1 - sum3 < min) {
                    min = sum1 - sum3;
                }

                if (sum1 == sum3 && sum2 < sum1 && sum1 - sum2 < min) {
                    min = sum1 - sum2;
                }

                if (sum2 == sum3 && sum1 < sum2 && sum2 - sum1 < min) {
                    min = sum2 - sum1;
                }

                System.out.println(min);
                if (min != Long.MAX_VALUE) {
                    if (result < 0 || min < result) {
                        result = min;
                    }
                }
            }
        }

        return result;
    }

    public static void readQuery() {
        int n = scanner.nextInt();

        // read nodes
        nodes = new Node[n];
        for (int i = 0; i < n; i++) {
            long c = scanner.nextLong();
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
        final long data;
        long sum;
        Node parent;

        public Node(int id, long data) {
            this.id = id;
            this.data = data;
            this.sum = data;
            this.parent = null;
        }

        public void addParent(Node parent) {
            if (this.parent != null) {
                //rewire old parents to be the children
                List<Node> stack = new ArrayList<>();

                Node p = this;
                while (p != null) {
                    stack.add(p);
                    p = p.parent;
                }

                for (int i = stack.size() - 1; i > 0; i--) {
                    p = stack.get(i);
                    p.parent = stack.get(i - 1);
                    p.sum -= p.parent.sum;
                    p.parent.sum += p.sum;
                }

                if (this.sum < 0) {
                    throw new RuntimeException("Sum overflow at node: " + this.id);
                }
            }

            this.parent = parent;

            while (parent != null) {
                parent.sum += this.sum;
                if (parent.sum < 0) {
                    throw new RuntimeException("Sum overflow at node: " + parent.id);
                }

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
