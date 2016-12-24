import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

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
            System.out.println(minMovesToZero(n, factorMap));
        }
    }

    public static int minMovesToZero(int n, Map<Integer, Entry> factorMap) {
        int c = 0;

        while (n > 0) {
            if (n == 1) {
                c++;
                break;
            }

            Entry e = factorMap.get(n);

            if (Math.min(e.a, e.b) == 1) {
                n--;
            } else {
                n = Math.max(e.a, e.b);
            }
            c++;
        }

        return c;
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
