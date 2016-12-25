import java.util.*;


public class DownToZero {

    private static Map<Integer, Set<Integer>> reduceMemoization = new HashMap<>();

    public static void main(String[] args) {
        Entry[] factorMap = factorMap();

        Scanner scanner = new Scanner(System.in);
        int q = scanner.nextInt();

        for (int i = 0; i < q; i++) {
            int n = scanner.nextInt();
            System.out.println(solve(n, factorMap));
        }
    }

    public static int solve(int n, Entry[] factorMap) {
        Queue<Entry> queue = new LinkedList<>();
        queue.add(new Entry(n, 0));
        int result = 0;

        if (n == 0) {
            return 0;
        }

        Map<Integer, Boolean> seen = new HashMap<>();

        while (!queue.isEmpty()) {
            Entry l = queue.remove();

            if (l.a == 1) {
                result = 1 + l.b;
                break;
            }

            Set<Integer> set = reduce(l.a, factorMap);

            if (!set.isEmpty()) {
                for (int i : set) {
                    if (!seen.containsKey(i)) {
                        queue.add(new Entry(i, l.b + 1));
                        seen.put(i, true);
                    }
                }
            }

            queue.add(new Entry(l.a - 1, l.b + 1));
            seen.put(l.a - 1, true);

            if (l.a - 1 == 1) {
                result = l.b + 2;
                break;
            }
        }

        return result;
    }

    public static Set<Integer> reduce(int n, Entry[] factorMap) {
        if (reduceMemoization.containsKey(n)) {
            return reduceMemoization.get(n);
        }

        List<Integer> primeFactors = new ArrayList<>();

        Entry e = factorMap[n];
        do {
            primeFactors.add(e.a);
            e = factorMap[e.b];
        } while (e != null);


        Set<Integer> set = new HashSet<>();
        for (int j = 1; j < (1 << primeFactors.size()) - 1; j++) {
            int f0 = 1;
            int f1 = 1;

            for (int k = 0; k < primeFactors.size(); k++) {
                if ((j >> k & 1) == 1) {
                    f1 *= primeFactors.get(k);
                } else {
                    f0 *= primeFactors.get(k);
                }
            }

            set.add(Math.max(f0, f1));
        }

        reduceMemoization.put(n, set);

        return set;
    }

    public static Entry[] factorMap() {
        Entry[] map = new Entry[1000001];

        for (int i = 2; i <= 1000000; i++) {
            if (map[i] == null) {
                for (int j = 1; i * j <= 1000000; j++) {
                    map[i * j] = new Entry(i, j);
                }
            }
        }

        return map;
    }

    public static class Entry {
        int a;
        int b;

        public Entry(int a, int b) {
            this.a = a;
            this.b = b;
        }
    }
}
