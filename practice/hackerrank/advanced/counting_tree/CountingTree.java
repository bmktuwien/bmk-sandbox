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

            solve(x1, y1, x2, y2, i);
        }
    }

    public static void solve(int x1, int y1, int x2, int y2, int run) {
        markPath(x1, y1, run);
        int cnt = countPath(x2, y2, run);

        System.out.println(cnt);
    }

    public static void markPath(int id1, int id2, int run) {
        Node n1 = nodes[id1];
        Node n2 = nodes[id2];

        if (n1.height > n2.height) {
            while (n1.height != n2.height) {
                n1.run = run;
                n1 = n1.parent;
            }
        } else if (n2.height > n1.height) {
            while (n1.height != n2.height) {
                n2.run = run;
                n2 = n2.parent;
            }
        }

        while (n1 != n2) {
            n1.run = run;
            n2.run = run;

            n1 = n1.parent;
            n2 = n2.parent;
        }

        n1.run = run;
    }

    public static int countDuplicates(Node n, int run) {
        int cnt = 0;

        for (Node dup : n.duplicates) {
            if (dup.run == run) {
                cnt++;
            }
        }

        return cnt;
    }
    public static int countPath(int id1, int id2, int run) {
        int cnt = 0;
        Node n1 = nodes[id1];
        Node n2 = nodes[id2];

        if (n1.height > n2.height) {
            while (n1.height != n2.height) {
                cnt += countDuplicates(n1, run);
                n1 = n1.parent;
            }
        } else if (n2.height > n1.height) {
            while (n1.height != n2.height) {
                cnt += countDuplicates(n2, run);
                n2 = n2.parent;
            }
        }

        while (n1 != n2) {
            cnt += countDuplicates(n1, run);
            cnt += countDuplicates(n2, run);

            n1 = n1.parent;
            n2 = n2.parent;
        }

        cnt += countDuplicates(n1, run);

        return cnt;
    }

    public static void readTree(int n) {
        // read nodes
        nodes = new Node[n];
        for (int i = 0; i < n; i++) {
            long c = scanner.nextLong();
            nodes[i] = new Node(i, c);
        }

        // read edges
        for (int i = 0; i < n - 1; i++) {
            int xId = scanner.nextInt() - 1;
            int yId = scanner.nextInt() - 1;

            Node parent = nodes[xId];
            Node child = nodes[yId];
            child.addParent(parent);
        }

        // add children
        Node root = null;
        for (Node node : nodes) {
            if (node.parent != null) {
                node.parent.addChild(node);
            } else {
                root = node;
            }
        }

        // update height information
        int height = 1;
        List<Node> nextLvlNodes = new ArrayList<>(root.children);

        while (!nextLvlNodes.isEmpty()) {
            List<Node> l = new ArrayList<>();

            for (Node node : nextLvlNodes) {
                node.height = height;
                l.addAll(node.children);
            }

            height++;
            nextLvlNodes = l;
        }

        Map<Long, HashSet<Node>> map = new HashMap<>();
        for (Node node : nodes) {
            if (map.containsKey(node.data)) {
                map.get(node.data).add(node);
            } else {
                HashSet<Node> set = new HashSet<>();
                set.add(node);
                map.put(node.data, set);
            }
        }

        for (HashSet<Node> duplicates : map.values()) {
            for (Node node : duplicates) {
                for (Node dup : duplicates) {
                    node.addDuplicate(dup);
                }
            }
        }
    }

    public static class Node {
        final int id;
        final long data;
        int height;
        int run;
        Node parent;
        List<Node> duplicates;
        HashSet<Node> children;

        public Node(int id, long data) {
            this.id = id;
            this.data = data;
            this.parent = null;
            this.height = 0;
            this.children = new HashSet<>();
            this.duplicates = new ArrayList<>();
            this.run = -1;
        }

        public void addDuplicate(Node node) {
            if (node != this) {
                duplicates.add(node);
            }
        }

        public void addChild(Node child) {
            this.children.add(child);
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
}
