import java.util.*;

public class KunduTree {

    private static long MOD = 1000000000L + 7;
    private static int totalCount = 0;

    private static Scanner scanner = new Scanner(System.in);
    private static HashSet<Edge> redEdges = new HashSet<>();
    private static Node[] nodes;

    public static void main(String[] args) {
        readTree();
        long result = count();

        System.out.println(result);
    }

    public static long count() {
        long result = 0;

        for (Edge e : redEdges) {
            Node c = e.getChildNode();

            long n1 = totalCount - c.cnt;
            long n2 = 0;

            for (Node n : c.children) {
                n2 += n.redCnt;
            }

            result += (n1 * n2);
            result = result % MOD;
        }

        return result;
    }

    public static void readTree() {
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

        for (Edge e : redEdges) {
            Node p = e.getChildNode();
            long c = p.cnt;
            p.redCnt = c;
            p.flag = true;
            p = p.parent;

            while (p != null && !p.flag) {
                Node p1 = p.parent;

                if (p1 != null) {
                    Edge e1 = Edge.createEdge(p.id, p1.id);
                    if (redEdges.contains(e1)) {
                        break;
                    }
                }

                p.redCnt += c;
                p = p1;
            }
        }

        Node root = null;
        for (Node node : nodes) {
            if (node.parent != null) {
                node.parent.addChild(node);
            } else {
                root = node;
            }
        }

        //return root;
    }

    public static class Node {
        final int id;
        long cnt;
        long redCnt;
        boolean flag;
        Node parent;
        HashSet<Node> children;

        public Node(int id) {
            this.id = id;
            this.cnt = 1;
            this.redCnt = 0;
            this.parent = null;
            this.children = new HashSet<>();
            this.flag = false;
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
}
