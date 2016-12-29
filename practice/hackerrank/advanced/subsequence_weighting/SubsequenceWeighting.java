import java.util.ArrayList;
import java.util.Random;
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

        /*Random r = new Random();
        while (true) {
            System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
            long[] a = new long[10];
            long[] w = new long[10];

            for (int i = 0; i < a.length; i++) {
                a[i] = r.nextInt(20);
                w[i] = r.nextInt(1000);
            }

            for (long l : a) {
                System.out.print(l + " ");
            }
            System.out.println();
            for (long l : w) {
                System.out.print(l + " ");
            }
            System.out.println();


            long max1 = solve(a, w);
            long max2 = bruteForce(a, w);

            if (max1 != max2) {
                break;
            }
        }*/
    }

    public static long bruteForce(long[] a, long[] w) {
        ArrayList<Long> result = null;
        long max = 0;

        for (int j = 0; j < (1 << a.length); j++) {
            ArrayList<Long> subA = new ArrayList<>();
            ArrayList<Long> subW = new ArrayList<>();

            for (int k = 0; k < a.length; k++) {
                if ((j >> k & 1) == 1) {
                    subA.add(a[k]);
                    subW.add(w[k]);
                }
            }

            long sum = 0;
            long tmp = -1;
            boolean invalid = false;
            for (int i = 0; i< subA.size(); i++) {
                if (subA.get(i) <= tmp) {
                    invalid = true;
                    break;
                }

                tmp = subA.get(i);
                sum += subW.get(i);
            }

            if (!invalid) {
                if (sum > max) {
                    max = sum;
                    result = subA;
                }
            }
        }

        System.out.println(max);
        for (long l : result) {
            System.out.print(l + " ");
        }
        System.out.println();

        return max;
    }

    public static long solve(long[] a, long[] w) {
        TreeMap<Long, Long> map = new TreeMap<>();

        for (int i = 0; i < a.length; i++) {
            Long key = map.floorKey(a[i]);
            Long wSum = -1L;

            if (key == null) {
                wSum = w[i];
                map.put(a[i], w[i]);
            } else {
                if (key != a[i]) {
                    wSum = map.get(key) + w[i];
                    map.put(a[i], wSum);
                } else {
                    Long lowerKey = map.lowerKey(a[i]);
                    if (lowerKey != null) {
                        if (map.get(lowerKey) + w[i] > map.get(key)) {
                            wSum = map.get(lowerKey) + w[i];
                            map.put(key, wSum);
                        }
                    } else {
                        if (map.get(key) < w[i]) {
                            wSum = w[i];
                            map.put(key, wSum);
                        }
                    }
                }
            }

            if (wSum > 0) {
                for (Long k : new ArrayList<>(map.tailMap(a[i], false).keySet())) {
                    if (map.get(k) <= wSum) {
                        map.remove(k);
                    }
                }
            }
        }

        long max = map.lastEntry().getValue();

        System.out.println(max);
        return max;
    }
}
