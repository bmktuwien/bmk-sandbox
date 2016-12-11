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
        System.out.println(edges.length);

        // brute force all edge pairs
        for (int i = 0; i < edges.length; i++) {
            for (int j = i + 1; j < edges.length; j++) {
                Map<Integer, Long> update = new HashMap<>();

                Edge e1 = edges[i];
                Edge e2 = edges[j];

                // first cut
                Node child = nodes[e1.xId].sum < nodes[e1.yId].sum ? nodes[e1.xId] : nodes[e1.yId];
                long sum1 = child.sum;

                // update sums after first cut
                Node parent = child.parent;
                while (parent != null) {
                    update.put(parent.id, parent.sum - sum1);
                    parent = parent.parent;
                }

                // second cut
                Node child2 = nodes[e2.xId].sum < nodes[e2.yId].sum ? nodes[e2.xId] : nodes[e2.yId];
                long sum2 = update.containsKey(child2.id) ? update.get(child2.id) : child2.sum;

                // calculate final sum
                parent = child2.parent;
                while (parent.parent != null) {
                    parent = parent.parent;
                }

                long sum3 = update.containsKey(parent.id) ? update.get(parent.id) - sum2 : parent.sum - sum2;

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
                //rewire old parents to be the children
                List<Node> stack = new ArrayList<>();

                Node p = this;
                while (p != null) {
                    stack.add(p);
                    p = p.parent;
                }

                long sum = 0;
                for (int i = stack.size() - 1; i > 0; i--) {
                    p = stack.get(i);
                    sum += p.data;
                    p.sum = sum;
                    p.parent = stack.get(i - 1);
                }

                this.sum += sum;
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
