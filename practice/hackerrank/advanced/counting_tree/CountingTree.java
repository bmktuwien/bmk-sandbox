import java.util.*;

/**
 * Created by bmk on 12/31/16.
 */
public class CountingTree {

    private static Scanner scanner = new Scanner(System.in);
    private static Node[] nodes;
    private static int[] heights;
    private static int[] parents;
    private static int[] runMarks;
    private static int[][] duplicates;

    public static void main(String[] args) {
        int n = scanner.nextInt();
        int q = scanner.nextInt();
        readTree(n);

        for (int i = 0; i < q; i++) {
            int x1 = scanner.nextInt() - 1;
            int y1 = scanner.nextInt() - 1;
            int x2 = scanner.nextInt() - 1;
            int y2 = scanner.nextInt() - 1;

            solve(x1, y1, x2, y2, i+1);
        }
    }

    public static void solve(int x1, int y1, int x2, int y2, int run) {
        markPath(x2, y2, run);
        int cnt = countPath(x1, y1, run);

        System.out.println(cnt);
    }

    public static void markPath(int id1, int id2, int run) {
        int n1 = id1;
        int n2 = id2;

        int h1 = heights[n1];
        int h2 = heights[n2];

        if (h1 > h2) {
            while (h1 != h2) {
                runMarks[n1] = run;
                n1 = parents[n1];
                h1--;
            }
        } else if (h2 > h1) {
            while (h1 != h2) {
                runMarks[n2] = run;
                n2 = parents[n2];
                h2--;
            }
        }

        while (n1 != n2) {
            runMarks[n1] = run;
            runMarks[n2] = run;

            n1 = parents[n1];
            n2 = parents[n2];
        }

        runMarks[n1] = run;
    }

    public static int countDuplicates(int nodeId, int run) {
        int cnt = 0;

        if (duplicates[nodeId] != null) {
            for (int dup : duplicates[nodeId]) {
                if (runMarks[dup] == run) {
                    cnt++;
                }
            }
        }

        return cnt;
    }

    public static int countPath(int id1, int id2, int run) {
        int cnt = 0;
        int n1 = id1;
        int n2 = id2;

        int h1 = heights[n1];
        int h2 = heights[n2];

        if (h1 > h2) {
            while (h1 !=h2) {
                cnt += countDuplicates(n1, run);
                n1 = parents[n1];
                h1--;
            }
        } else if (h2 > h1) {
            while (h1 != h2) {
                cnt += countDuplicates(n2, run);
                n2 = parents[n2];
                h2--;
            }
        }

        while (n1 != n2) {
            cnt += countDuplicates(n1, run);
            cnt += countDuplicates(n2, run);

            n1 = parents[n1];
            n2 = parents[n2];
        }

        cnt += countDuplicates(n1, run);

        return cnt;
    }

    public static void readTree(int n) {
        // read nodes
        nodes = new Node[n];
        heights = new int[n];
        parents = new int[n];
        runMarks = new int[n];
        duplicates = new int[n][];

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

        for (Node node : nodes) {
            if (node.parent != null) {
                parents[node.id] = node.parent.id;
            } else {
                parents[node.id] = -1;
            }
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
                heights[node.id] = height;
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

        for (HashSet<Node> dups : map.values()) {
            for (Node node : dups) {
                if (dups.size() > 1) {
                    duplicates[node.id] = new int[dups.size() - 1];

                    int i = 0;
                    for (Node dup : dups) {
                        if (dup != node) {
                            duplicates[node.id][i] = dup.id;
                            i++;
                        }
                    }
                }
            }
        }
    }

    public static class Node {
        final int id;
        final long data;
        Node parent;
        HashSet<Node> children;

        public Node(int id, long data) {
            this.id = id;
            this.data = data;
            this.parent = null;
            this.children = new HashSet<>();
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
}
