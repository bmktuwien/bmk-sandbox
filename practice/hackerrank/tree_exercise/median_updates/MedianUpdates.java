import java.util.Scanner;
import java.util.TreeMap;

public class MedianUpdates {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();
        TreeMap<Integer, Integer> map = new TreeMap<>();

        Integer median = null;

        for (int i = 0; i < n; i++) {
            String s = scanner.next();
            int v = scanner.nextInt();

            if (median == null) {
                if (s.equals("r")) {
                    System.out.println("Wrong!");
                } else {
                    map.put(v, v);
                    median = v;
                    System.out.println(v);
                }
            } else {
                if (s.equals("r")) {
                    Integer k = map.remove(v);

                    if (map.isEmpty()) {
                        System.out.println("Wrong!");
                    } else {
                        if (k == null) {
                            System.out.println("Wrong!");
                        } else {
                            // size has changed
                            if (map.size() % 2 == 0) {
                                if (v <= median) {
                                    median = map.higherKey(median);
                                }
                            } else {
                                if (v >= median) {
                                    median = map.lowerKey(median);
                                }
                            }

                            if (map.size() % 2 == 0) {
                                int median2 = map.lowerKey(median);
                                printMeanMedian(median2, median);
                            } else {
                                System.out.println(median);
                            }
                        }
                    }

                } else {
                    Integer k = map.put(v, v);

                    if (k == null) {
                        // size has changed
                        if (map.size() % 2 == 0) {
                            if (v > median) {
                                median = map.higherKey(median);
                            }
                        } else {
                            if (v < median) {
                                median = map.lowerKey(median);
                            }
                        }
                    }

                    if (map.size() % 2 == 0) {
                        int median2 = map.lowerKey(median);
                        printMeanMedian(median2, median);
                    } else {
                        System.out.println(median);
                    }
                }
            }
        }
    }

    public static void printMeanMedian(int m1, int m2) {
        if ((m1 + m2) % 2 == 0) {
            System.out.println((m1 + m2) / 2);
        } else {
            System.out.println(String.format("%.1f", (m1 + m2) / 2.0d));
        }
    }
}
