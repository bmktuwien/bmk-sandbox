import java.util.*;

/**
 * Created by bmk on 12/11/16.
 */
public class Solution {

    private static Scanner scanner = new Scanner(System.in);
    private static Node[] nodes;
    private static long totalSum;

    private static Map<Long, Boolean> lookupTable;
    private static List<Node> candidates;

    public static void main(String[] args) {
        int q = scanner.nextInt();

        for (int i = 0; i < q; i++) {
            readQuery();
            buildAux();
            long result = solve();
            System.out.println(result);
        }
    }

    public static long solve() {
        for (Node candidate : candidates) {
            long diff = totalSum - candidate.sum;

            if (diff % 2 == 0) {
                if (lookupTable.containsKey(diff / 2)) {
                    System.out.println(diff / 2 - candidate.sum);
                    break;
                }
            }
        }

        return -1;
    }

    public static void buildAux() {
        long threshold = totalSum / 3;
        lookupTable = new HashMap<>();
        candidates = new ArrayList<>();

        for (Node node : nodes) {
            lookupTable.put(node.sum, true);

            if (node.sum < threshold) {
                candidates.add(node);
            }
        }

        candidates.sort(Comparator.comparing(Node::getSum));
        Collections.reverse(candidates);
    }

    public static void readQuery() {
        int n = scanner.nextInt();

        totalSum = 0;
        // read nodes
        nodes = new Node[n];
        for (int i = 0; i < n; i++) {
            long c = scanner.nextLong();
            nodes[i] = new Node(i, c);
            totalSum += c;
        }

        // read edges
        Edge[] edges = new Edge[n - 1];
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

        //System.out.println("Total sum: " + totalSum);
        //System.out.println("Root sum: " + root.sum);
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

        public long getSum() {
            return sum;
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
