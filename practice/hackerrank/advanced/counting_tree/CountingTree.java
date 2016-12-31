import java.util.*;

/**
 * Created by bmk on 12/31/16.
 */
public class CountingTree {

    private static Scanner scanner = new Scanner(System.in);
    private static Node[] nodes;

    public static void main(String[] args) {
        int n = scanner.nextInt();
        int q = scanner.nextInt();
        readTree(n);

        for (int i = 0; i < q; i++) {
            int x1 = scanner.nextInt() - 1;
            int y1 = scanner.nextInt() - 1;
            int x2 = scanner.nextInt() - 1;
            int y2 = scanner.nextInt() - 1;

            solve(x1, y1, x2, y2);
        }
    }

    public static void solve(int x1, int y1, int x2, int y2) {
        List<Node> path1 = findPath(x1, y1);
        List<Node> path2 = findPath(x2, y2);

        int cnt = 0;
        Map<Long, HashSet<Integer>> map = new HashMap<>();

        for (Node n : path1) {
            if (map.containsKey(n.data)) {
                map.get(n.data).add(n.id);
            } else {
                HashSet<Integer> s = new HashSet<>();
                s.add(n.id);
                map.put(n.data, s);
            }
        }

        for (Node n: path2) {
            if (map.containsKey(n.data)) {
                cnt += map.get(n.data).size();

                if (map.get(n.data).contains(n.id)) {
                    cnt--;
                }
            }
        }

        System.out.println(cnt);
    }
    public static List<Node> findPath(int id1, int id2) {
        Node n1 = nodes[id1];
        Node n2 = nodes[id2];

        List<Node> path = new ArrayList<>();
        Map<Integer, Boolean> seen = new HashMap<>();

        Node tmp = n1;
        while (tmp != null) {
            seen.put(tmp.id, true);
            tmp = tmp.parent;
        }

        while (n2 != null) {
            if (!seen.containsKey(n2.id)) {
                path.add(n2);
                n2 = n2.parent;
            } else {
                path.add(n2);
                break;
            }
        }

        while (n1 != n2) {
            path.add(n1);
            n1 = n1.parent;
        }

        return path;
    }

    public static void readTree(int n) {
        // read nodes
        nodes = new Node[n];
        for (int i = 0; i < n; i++) {
            long c = scanner.nextLong();
            nodes[i] = new Node(i, c);
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
    }

    public static class Node {
        final int id;
        final long data;
        Node parent;

        public Node(int id, long data) {
            this.id = id;
            this.data = data;
            this.parent = null;
        }

        public void addParent(Node parent) {
            if (this.parent != null) {
                List<Node> stack = new ArrayList<>();

                Node p = this;
                while (p != null) {
                    stack.add(p);
                    p = p.parent;
                }

                for (int i = stack.size() - 1; i > 0; i--) {
                    p = stack.get(i);
                    p.parent = stack.get(i - 1);
                }
            }

            this.parent = parent;

            while (parent != null) {
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
