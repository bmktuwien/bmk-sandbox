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

    public static List<Pair<Edge,Integer>> findRed(Node node) {
        List<Pair<Edge,Integer>> result = new ArrayList<>();

        Stack<Node> s = new Stack<>();
        Stack<Integer> i = new Stack<>();
        s.add(node);
        i.add(0);

        while (!s.isEmpty()) {
            Node n = s.pop();
            int cnt = i.pop();

            for (Node c : n.children) {
                Edge e = Edge.createEdge(n.id, c.id);

                if (redEdges.contains(e)) {
                    Pair<Edge,Integer> r = new Pair<>();
                    r.elem1 = e;
                    r.elem2 = cnt;
                    result.add(r);
                } else {
                    s.push(c);
                    i.push(cnt+1);
                }
            }
        }

        return result;
    }

    public static void solve(Node root) {
        LinkedList<Pair<List<Pair<Edge,Integer>>,Edge>> queue = new LinkedList<>();
        Pair<List<Pair<Edge,Integer>>,Edge> p = new Pair<>();
        p.elem1 = findRed(root);
        p.elem2 = null;
        queue.add(p);


        while (!queue.isEmpty()) {
            Pair<List<Pair<Edge,Integer>>,Edge> p0 = queue.removeFirst();

            for (Pair<Edge,Integer> p1 : p0.elem1) {
                Edge e = p1.elem1;
                List<Pair<Edge,Integer>> tmp = findRed(e.getChildNode());

                if (!tmp.isEmpty()) {
                    Pair<List<Pair<Edge,Integer>>,Edge> p2 = new Pair<>();
                    p2.elem1 = tmp;
                    p2.elem2 = e;

                    queue.add(p2);
                }
            }
        }
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
