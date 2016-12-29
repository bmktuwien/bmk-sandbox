import java.util.ArrayList;
import java.util.Scanner;
import java.util.TreeMap;

/**
 * Created by bmk on 12/29/16.
 */
public class SubsequenceWeighting {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        int t = scanner.nextInt();

        for (int i = 0; i < t; i++) {
            int n = scanner.nextInt();
            long[] a = new long[n];
            long[] w = new long[n];
            for (int j = 0; j < n; j++) {
                a[j] = scanner.nextLong();
            }
            for (int j = 0; j < n; j++) {
                w[j] = scanner.nextLong();
            }

            solve(a, w);
        }
    }

    public static void solve(long[] a, long[] w) {
        TreeMap<Long, Entry> map = new TreeMap<>();

        for (int i = 0; i < a.length; i++) {
            Long key = map.floorKey(a[i]);

            if (key == null) {
                Entry e = new Entry();
                e.wSum = w[i];
                e.lastWeight = w[i];
                map.put(a[i], e);
            } else {
                Entry e = map.get(key);

                if (key != a[i]) {
                    Entry newE = new Entry();
                    newE.wSum = e.wSum + w[i];
                    newE.lastWeight = w[i];
                    map.put(a[i], newE);

                    // cleanup higher keys
                    for (Long k : new ArrayList<>(map.tailMap(a[i], false).keySet())) {
                        if (map.get(k).wSum <= newE.wSum) {
                            map.remove(k);
                        }
                    }
                } else {
                    if (w[i] > e.lastWeight) {
                        e.wSum = e.wSum - e.lastWeight + w[i];
                        e.lastWeight = w[i];
                    }

                    // cleanup higher keys
                    for (Long k : new ArrayList<>(map.tailMap(a[i], false).keySet())) {
                        if (map.get(k).wSum <= e.wSum) {
                            map.remove(k);
                        }
                    }
                }
            }
        }

        long max = 0;
        for (Entry e : map.values()) {
            if (e.wSum > max) {
                max = e.wSum;
            }
        }

        System.out.println(max);
    }

    public static class Entry {
        long wSum;
        long lastWeight;
    }
}
