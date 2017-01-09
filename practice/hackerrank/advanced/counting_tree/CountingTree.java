import java.util.*;

/**
 * Created by bmk on 12/31/16.
 */
public class CountingTree {

    private static Scanner scanner = new Scanner(System.in);
    private static HashSet<Long> duplicatedValues = new HashSet<>();
    private static Node[] nodes;

    public static void main(String[] args) {
        int n = scanner.nextInt();
        int q = scanner.nextInt();
        readTree(n);

        /*for (int i = 0; i < q; i++) {
            int x1 = scanner.nextInt() - 1;
            int y1 = scanner.nextInt() - 1;
            int x2 = scanner.nextInt() - 1;
            int y2 = scanner.nextInt() - 1;

            solve(x1, y1, x2, y2, i+1);
        }*/
    }

    public static void solve(int x1, int y1, int x2, int y2, int run) {
        //TODO: implement me!
    }

    public static void readTree(int n) {
        // read nodes
        nodes = new Node[n];

        for (int i = 0; i < n; i++) {
            long c = scanner.nextLong();
            nodes[i] = new Node(i, c);
        }

        // build tree
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

        Map<Long, Integer> map = new HashMap<>();
        for (Node node : nodes) {
            if (map.containsKey(node.data)) {
                map.put(node.data, map.get(node.data) + 1);
            } else {
                map.put(node.data, 1);
            }
        }

        for (Long data : map.keySet()) {
            if (map.get(data) > 1) {
                duplicatedValues.add(data);
            }
        }

        List<Node> nextLvlNodes = new ArrayList<>();
        List<HashSet<DupParent>> nextLvlParents = new ArrayList<>();
        nextLvlNodes.add(root);
        nextLvlParents.add(new HashSet<>());

        while (!nextLvlNodes.isEmpty()) {
            List<Node> l = new ArrayList<>();
            List<HashSet<DupParent>> p = new ArrayList<>();

            for (int i = 0; i < nextLvlNodes.size(); i++) {
                Node node = nextLvlNodes.get(i);
                HashSet<DupParent> parents = nextLvlParents.get(i);

                if (duplicatedValues.contains(node.data)) {
                    int idx = 0;
                    for (Node c : node.children) {
                        HashSet<DupParent> copy = new HashSet<>(parents);
                        copy.add(new DupParent(c, idx));
                        idx++;

                        l.add(c);
                        p.add(copy);
                    }
                } else {
                    for (Node c : node.children) {
                        l.add(c);
                        p.add(parents);
                    }
                }
            }

            nextLvlNodes = l;
            nextLvlParents = p;
        }

    }

    public static class Node {
        final int id;
        final long data;
        Node parent;
        HashSet<Node> children;
        HashSet<DupParent> duplicatedParents;

        public Node(int id, long data) {
            this.id = id;
            this.data = data;
            this.parent = null;
            this.children = new HashSet<>();
        }

        public void setDuplicatedParents(HashSet<DupParent> dupParents) {
            this.duplicatedParents = dupParents;
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
        }
    }

    public static class DupParent {
        final Node node;
        final int idx;

        public DupParent(Node node, int idx) {
            this.node = node;
            this.idx = idx;
        }
    }
}
