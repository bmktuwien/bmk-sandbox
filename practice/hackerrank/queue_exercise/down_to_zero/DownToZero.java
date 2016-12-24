import java.util.*;

/**
 * Created by bmk on 12/24/16.
 */
public class DownToZero {

    public static void main(String[] args) {
        Map<Integer, Entry> factorMap = factorMap();
        Map<Integer, Integer> minMap = minMap(factorMap);

        Scanner scanner = new Scanner(System.in);
        int q = scanner.nextInt();

        for (int i = 0; i < q; i++) {
            int n = scanner.nextInt();
            System.out.println(minMap.get(n));
        }
    }

    public static Map<Integer, Integer> minMap(Map<Integer, Entry> factorMap) {
        Map<Integer, Integer> map = new HashMap<>();
        map.put(0, 0);
        map.put(1, 1);

        for (int i = 2; i < 1000000; i++) {
            List<Integer> primeFactors = new ArrayList<>();

            Entry e = factorMap.get(i);
            do {
                primeFactors.add(e.a);
                e = factorMap.get(e.b);
            } while (e != null);

            int min = map.get(i - 1);

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

                int t = map.get(Math.max(f0, f1));
                if (t < min) {
                    min = t;
                }
            }

            map.put(i, min + 1);
        }

        return map;
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
