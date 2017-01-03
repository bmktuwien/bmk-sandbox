import java.util.Scanner;

/**
 * Created by bmk on 1/3/17.
 */
public class ComponentsGraph {

    private static int[] bs;
    private static int[] parents;
    private static int[] ranks;

    public static void init(int n) {
        bs = new int[n];
        parents = new int[n];
        ranks = new int[n];

        for (int i = 0; i < parents.length; i++) {
            bs[i] = -1;
        }

        for (int i = 0; i < parents.length; i++) {
            parents[i] = i;
        }

        for (int i = 0; i < ranks.length; i++) {
            ranks[i] = 1;
        }
    }

    public static int find(int i) {
        // path compression
        if (parents[i] != i) {
            parents[i] = find(parents[i]);
        }

        return parents[i];
    }

    public static void union(int x, int y) {
        int i = find(x);
        int j = find(y);

        // already in the same set, return
        if (i == j) {
            return;
        }

        if (ranks[i] > ranks[j]) {
            parents[j] = i;
            ranks[i] += ranks[j];
        } else {
            parents[i] = j;
            ranks[j] += ranks[i];
        }
    }

    public static void main(String[] args) {
        Scanner s = new Scanner(System.in);

        int n = s.nextInt();

        init(n);

        for (int i = 0; i < n; i++) {
            int g = s.nextInt() - 1;
            int b = s.nextInt() - n - 1;

            if (bs[b] == -1) {
                bs[b] = g;
                ranks[find(g)]++;
            } else {
                union(g, bs[b]);
            }
        }

        //find min/max ranks;
        int min = Integer.MAX_VALUE;
        int max = Integer.MIN_VALUE;

        for (int i = 0; i < n; i++) {
            int r = ranks[find(i)];

            if (r < min && r > 1) {
                min = r;
            }

            if (r > max) {
                max = r;
            }
        }

        System.out.println(min + " " + max);
    }

}
