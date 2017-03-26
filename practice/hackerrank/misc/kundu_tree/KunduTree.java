import java.util.*;

public class KunduTree {

    private static Scanner scanner = new Scanner(System.in);

    public static void main(String[] args) {
        Node root = readTree();
        System.out.println(root.cnt);
    }

    public static long solve() {
        return -1;
    }


    public static Node readTree() {
        int n = scanner.nextInt();

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
            scanner.next();

            edges[i] = new Edge(xId, yId);
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

        public Edge(int xId, int yId) {
            this.xId = xId;
            this.yId = yId;
        }
    }
}
