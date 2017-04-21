import java.util.*;

public class KunduTree {

    private static long MOD = 1000000000L + 7;
    private static int totalCount = 0;

    private static Scanner scanner = new Scanner(System.in);
    private static HashSet<Edge> redEdges = new HashSet<>();

    public static void main(String[] args) {
        Node root = readTree();
        long result = solve(root);
        System.out.println(result);
    }

    public static long solve(Node root) {
        return traverse(root, null, 0);
    }

    public static long traverse(Node n, Node prevRed, long result) {
        for (Node child : n.children) {
            Edge e = Edge.createEdge(n.id, child.id);

            if (redEdges.contains(e)) {
                if (prevRed != null) {
                    long n1 = totalCount - prevRed.cnt;
                    long n2 = child.cnt;

                    System.out.println(n1 * n2);
                    result += (n1 * n2);
                    result = result % MOD;
                }

                result = traverse(child, child, result);
            } else {
                result = traverse(child, prevRed, result);
            }
        }

        return result;
    }

    public static Node readTree() {
        int n = scanner.nextInt();
        totalCount = n;

        // read nodes
        Node[] nodes = new Node[n];
        for (int i = 0; i < n; i++) {
            nodes[i] = new Node(i);
        }

        // read edges
        Edge[] edges = new Edge[n - 1];
        for (int i = 0; i < n - 1; i++) {
            int xId = scanner.nextInt() - 1;
            int yId = scanner.nextInt() - 1;
            String color = scanner.next();

            edges[i] = Edge.createEdge(xId, yId);

            if (color.equals("r")) {
                redEdges.add(edges[i]);
            }
        }

        // build tree
        for (Edge e : edges) {
            Node parent = nodes[e.xId];
            Node child = nodes[e.yId];

            child.addParent(parent);
        }

        Node root = null;
        for (Node node : nodes) {
            if (node.parent != null) {
                node.parent.addChild(node);
            } else {
                root = node;
            }
        }

        return root;
    }

    public static class Node {
        final int id;
        long cnt;
        Node parent;
        HashSet<Node> children;

        public Node(int id) {
            this.id = id;
            this.cnt = 1;
            this.parent = null;
            this.children = new HashSet<>();
        }

        public void addChild(Node child) {
            this.children.add(child);
        }

        public void addParent(Node parent) {
            if (this.parent != null) {
                //rewire old parents ch2 be the children
                List<Node> stack = new ArrayList<>();

                Node p = this;
                while (p != null) {
                    stack.add(p);
                    p = p.parent;
                }

                for (int i = stack.size() - 1; i > 0; i--) {
                    p = stack.get(i);
                    p.parent = stack.get(i - 1);
                    p.cnt -= p.parent.cnt;
                    p.parent.cnt += p.cnt;
                }
            }

            this.parent = parent;

            while (parent != null) {
                parent.cnt += this.cnt;
                parent = parent.parent;
            }
        }
    }

    public static class Edge {
        final int xId;
        final int yId;

        private Edge(int xId, int yId) {
            this.xId = xId;
            this.yId = yId;
        }

        @Override
        public boolean equals(Object o) {
            if (o instanceof Edge) {
                Edge other = (Edge) o;
                return xId == other.xId && yId == other.yId;
            } else {
                return false;
            }
        }

        @Override
        public int hashCode() {
            return xId * 31 + yId;
        }

        public static Edge createEdge(int xId, int yId) {
            int xId1 = Math.min(xId, yId);
            int yId1 = Math.max(xId, yId);

            return new Edge(xId1, yId1);
        }
    }
}
