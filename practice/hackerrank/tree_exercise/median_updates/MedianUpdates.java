import java.util.HashMap;
import java.util.Scanner;
import java.util.TreeMap;

public class MedianUpdates {

    private static HashMap<Long, Long> map = new HashMap<>();

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();
        TreeMap<Key, Long> map = new TreeMap<>();

        Key median = null;

        for (long i = 0; i < n; i++) {
            String s = scanner.next();
            long v = scanner.nextInt();

            if (median == null) {
                if (s.equals("r")) {
                    System.out.println("Wrong!");
                } else {
                    Key k = getKeyForAdd(v);
                    map.put(k, v);
                    median = k;
                    System.out.println(median.value);
                }
            } else {
                if (s.equals("r")) {
                    Key k = getKeyForRemove(v);

                    if (k == null) {
                        System.out.println("Wrong!");
                        continue;
                    }

                    map.remove(k);

                    if (map.isEmpty()) {
                        median = null;
                        System.out.println("Wrong!");
                    } else {
                        if (map.size() % 2 == 0) {
                            if (k.compareTo(median) <= 0) {
                                median = map.higherKey(median);
                            }
                        } else {
                            if (k.compareTo(median) >= 0) {
                                median = map.lowerKey(median);
                            }
                        }

                        if (map.size() % 2 == 0) {
                            Key median2 = map.lowerKey(median);
                            printMeanMedian(median2, median);
                        } else {
                            System.out.println(median.value);
                        }
                    }

                } else {
                    Key k = getKeyForAdd(v);
                    map.put(k, v);

                    if (map.size() % 2 == 0) {
                        if (k.compareTo(median) >= 0) {
                            median = map.higherKey(median);
                        }
                    } else {
                        if (k.compareTo(median) < 0) {
                            median = map.lowerKey(median);
                        }
                    }

                    if (map.size() % 2 == 0) {
                        Key median2 = map.lowerKey(median);
                        printMeanMedian(median2, median);
                    } else {
                        System.out.println(median.value);
                    }
                }
            }
        }
    }

    public static void printMeanMedian(Key m1, Key m2) {
        if ((m1.value + m2.value) % 2 == 0) {
            System.out.println((m1.value + m2.value) / 2);
        } else {
            System.out.println(String.format("%.1f", (m1.value + m2.value) / 2.0d));
        }
    }

    public static Key getKeyForAdd(long value) {
        if (map.containsKey(value)) {
            long c = map.get(value);
            map.put(value, c+1);

            return new Key(value, c+1);
        } else {
            map.put(value, 1L);
            return new Key(value, 1L);
        }
    }

    public static Key getKeyForRemove(long value) {
        if (map.containsKey(value)) {
            long c = map.get(value);

            if (c > 1) {
                map.put(value, c-1);
            } else {
                map.remove(value);
            }

            return new Key(value, c);
        } else {
            return null;
        }
    }

    public static class Key implements Comparable<Key> {
        Long value;
        Long count;

        public Key(Long value, Long count) {
            this.value = value;
            this.count = count;
        }

        @Override
        public int compareTo(Key other) {
            int i = value.compareTo(other.value);

            if (i != 0) {
                return i;
            }

            return count.compareTo(other.count);
        }
    }
}
