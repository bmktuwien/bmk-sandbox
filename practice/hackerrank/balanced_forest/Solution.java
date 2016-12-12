import java.util.*;

/**
 * Created by bmk on 12/11/16.
 */
public class Solution {

    private static Scanner scanner = new Scanner(System.in);
    private static Node[] nodes;
    private static Edge[] edges;
    private static long totalSum;
    private static Node root;

    public static void main(String[] args) {
        int q = scanner.nextInt();

        for (int i = 0; i < q; i++) {
            readQuery();
            Map<Long, List<Node>> candidates = findCandidates();
            long result = solve(candidates);
            System.out.println(result);
        }
    }

    public static long solve(Map<Long, List<Node>> candidates) {
        long result = -1;

        for (long sum : candidates.keySet()) {
            if (candidates.get(sum).size() > 1) {
                long cw = 3 * sum - totalSum;
                if (result == -1 || cw < result) {
                    result = cw;
                }
            } else if (candidates.containsKey(sum * 2)) {
                System.out.println(sum);
            }
        }

        return result;
    }

    public static Map<Long, List<Node>> findCandidates() {
        long threshold = totalSum / 3;
        HashMap<Long, List<Node>> candidates = new HashMap<>();

        for (Node node : nodes) {
            if (node.sum > threshold) {
                if (candidates.containsKey(node.sum)) {
                    candidates.get(node.sum).add(node);
                } else {
                    List<Node> l = new ArrayList<>();
                    l.add(node);
                    candidates.put(node.sum, l);
                }
            }
        }

        return candidates;
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

        // find root
        /*for (Node node : nodes) {
            if (node.parent == null) {
                root = node;
            }
        }*/

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
