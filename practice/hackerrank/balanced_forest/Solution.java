import java.util.*;

/**
 * Created by bmk on 12/11/16.
 */
public class Solution {

    private static Scanner scanner = new Scanner(System.in);
    private static Node[] nodes;
    private static long totalSum;
    private static Map<Long, List<Node>> lookupTable;

    /**
     * REMARKS after studying editorial solution:
     *
     * - Use DFS timestamps to determine fast ancestor relationship!
     * - Build tree once (WITHOUT parent/child rewiring at each step!!!)
     *   and calculate the sums in one pass
     * - Save everything aux information in a map for fast lookup!
     *
     */
    public static void main(String[] args) {
        int q = scanner.nextInt();

        for (int i = 0; i < q; i++) {
            readQuery();
            buildAux();
            long result = solve();
            System.out.println(result);
        }
    }

    public static boolean existsSubTreeWithSum(long sum, Node node, boolean connected) {
        List<Node> nodes = lookupTable.get(sum);
        HashMap<Integer, Node> tabu = new HashMap<>();

        if (!connected) {
            Node n = node.parent;

            while (n != null) {
                if (n.sum - node.sum == sum) {
                    return true;
                }

                tabu.put(n.id, n);
                n = n.parent;
            }
        }

        boolean flag = false;
        if (nodes != null) {
            for (Node n : nodes) {
                if (tabu.containsKey(n.id) || n.id == node.id) {
                    continue;
                }

                flag = true;
                while (n != null) {
                    if (n.parent == node) {
                        return connected;
                    }

                    n = n.parent;
                }
            }

            return flag && !connected;
        } else {
            return false;
        }

    }

    public static long solve() {
        long result = -1;

        for (Node node : nodes) {
            if (node.parent == null) {
                if (node.sum % 2 == 0) {
                    if (lookupTable.containsKey(node.sum / 2)) {
                        long cw = node.sum / 2;
                        if (result == -1 || cw < result) {
                            result = cw;
                        }
                    }
                }

                if (node.sum % 3 == 0) {
                    if (lookupTable.getOrDefault(node.sum / 3, new ArrayList<>()).size() > 1) {
                        return 0;
                    }
                }

                continue;
            }

            long diff = totalSum - node.sum;

            if (diff <= node.sum) {
                if (node.sum % 2 == 0 && node.sum / 2 >= diff) {
                    if (existsSubTreeWithSum(node.sum / 2, node, true)) {
                        long cw = node.sum / 2 - diff;

                        if (result == -1 || cw < result) {
                            result = cw;
                        }
                    }
                }

                if (2 * diff > node.sum) {
                    if (existsSubTreeWithSum(diff, node, true)) {
                        long cw = 2 * diff - node.sum;

                        if (result == -1 || cw < result) {
                            result = cw;
                        }
                    }
                }
            } else {
                if (diff % 2 == 0  && diff / 2 >= node.sum) {
                    if (existsSubTreeWithSum(diff / 2, node, false)) {
                        long cw = diff / 2 - node.sum;

                        if (result == -1 || cw < result) {
                            result = cw;
                        }
                    }
                }

                if (2 * node.sum > diff) {
                    if (existsSubTreeWithSum(node.sum, node, false)) {
                        long cw = 2 * node.sum - diff;

                        if (result == -1 || cw < result) {
                            result = cw;
                        }
                    }
                }
            }
        }

        return result;
    }

    public static void buildAux() {
        lookupTable = new HashMap<>();

        for (Node node : nodes) {
            if (lookupTable.containsKey(node.sum)) {
                lookupTable.get(node.sum).add(node);
            } else {
                List<Node> l = new ArrayList<>();
                l.add(node);
                lookupTable.put(node.sum, l);
            }
        }
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

        /*Node root = null;
        for (Node node : nodes) {
            if (node.parent == null) {
                root = node;
            }
        }
        System.out.println("Total sum: " + totalSum);
        System.out.println("Root sum: " + root.sum);*/
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
