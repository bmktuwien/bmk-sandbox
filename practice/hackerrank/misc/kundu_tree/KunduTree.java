import java.util.*;

public class KunduTree {

    private static long MOD = 1000000000L + 7;
    private static int totalCount = 0;

    private static Scanner scanner = new Scanner(System.in);
    private static HashSet<Edge> redEdges = new HashSet<>();
    private static Node[] nodes;

    public static void main(String[] args) {
        Node root = readTree();
        solve(root);
    }

    public static void addReds(Edge redEdge) {
        Node n = redEdge.getChildNode();
        Node p = redEdge.getParentNode();
        int depth = 0;
        p.addRed(n.id, depth);
        Node p2 = p.parent;

        while (p2 != null) {
            depth++;

            Edge e = Edge.createEdge(p.id, p2.id);
            if (redEdges.contains(e)) {
                break;
            } else {
                p2.addRed(n.id, depth);
            }

            p = p2;
            p2 = p2.parent;
        }
    }

    public static void solve(Node root) {
    }

    public static Node readTree() {
        int n = scanner.nextInt();
        totalCount = n;

        // read nodes
        nodes = new Node[n];
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

        // add reds
        for (Edge e : redEdges) {
            addReds(e);
        }

        // add children info
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
        HashMap<Integer, List<Integer>> reds;

        public Node(int id) {
            this.id = id;
            this.cnt = 1;
            this.parent = null;
            this.children = new HashSet<>();
            this.reds = new HashMap<>();
        }

        public void addChild(Node child) {
            this.children.add(child);
        }

        public void addRed(int childId, int depth) {
            if (!reds.containsKey(childId)) {
                reds.put(childId, new ArrayList<>());
            }

            reds.get(childId).add(depth);
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

        public Node getParentNode() {
            Node x = nodes[xId];
            Node y = nodes[yId];

            if (x.parent != null && x.parent.id == yId) {
                return y;
            } else {
                return x;
            }
        }

        public Node getChildNode() {
            Node x = nodes[xId];
            Node y = nodes[yId];

            if (x.parent != null && x.parent.id == yId) {
                return x;
            } else {
                return y;
            }
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

    public static class Pair<S,T> {
        public S elem1;
        public T elem2;
    }
}
