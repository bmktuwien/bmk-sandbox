import java.util.Scanner;

/**
 * Created by bmk on 1/3/17.
 */
public class MergingCommunities {

    private static int[] parents;
    private static int[] ranks;

    public static void init(int n) {
        parents = new int[n];
        ranks = new int[n];

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
        Scanner scanner = new Scanner(System.in);

        int n = scanner.nextInt();
        int q = scanner.nextInt();

        init(n);

        for (int i = 0; i < q; i++) {
            String cmd = scanner.next();

            switch (cmd) {
                case "M":
                    int x = scanner.nextInt();
                    int y = scanner.nextInt();
                    union(x-1, y-1);
                    break;
                case "Q":
                    int z = scanner.nextInt();
                    System.out.println(ranks[find(z-1)]);
                    break;
            }
        }
    }
}
