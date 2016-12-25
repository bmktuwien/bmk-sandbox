import java.util.*;

/**
 * Created by bmk on 12/24/16.
 */
public class DownToZero {

    public static void main(String[] args) {
        Map<Integer, Entry> factorMap = factorMap();

        Scanner scanner = new Scanner(System.in);
        int q = scanner.nextInt();

        for (int i = 0; i < q; i++) {
            int n = scanner.nextInt();
            System.out.println(solve(n, factorMap));
        }
    }

    public static int solve(int n, Map<Integer, Entry> factorMap) {
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

            List<Integer> primeFactors = new ArrayList<>();

            Entry e = factorMap.get(l.a);
            do {
                primeFactors.add(e.a);
                e = factorMap.get(e.b);
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

            for (int i : set) {
                if (!seen.containsKey(i)) {
                    queue.add(new Entry(i, l.b + 1));
                    seen.put(i, true);
                }
            }

            queue.add(new Entry(l.a - 1, l.b + 1));
            seen.put(l.a - 1, true);
        }


        return result;
    }

    public static Map<Integer, Entry> factorMap() {
        Map<Integer, Entry> map = new HashMap<>();

        for (int i = 2; i <= 1000000; i++) {
            if (!map.containsKey(i)) {
                for (int j = 1; i * j <= 1000000; j++) {
                    map.put(i * j, new Entry(i, j));
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
