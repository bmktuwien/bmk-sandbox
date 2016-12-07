import java.util.*;

/**
 * Created by bmk on 12/7/16.
 */
public class Problem4 {

    public static void main(String[] args) {
        List<int[]> test1 = Arrays.asList(
                new int[]{0, 1},
                new int[]{3, 5},
                new int[]{5, 8},
                new int[]{10, 12},
                new int[]{9, 10}
        );
        List<int[]> sol1 = mergeTimeWindows(test1);
        printTimeWindows(sol1);

        List<int[]> test2 = Arrays.asList(
                new int[]{1, 2},
                new int[]{2, 5},
                new int[]{9, 10}
        );
        List<int[]> sol2 = mergeTimeWindows(test2);
        printTimeWindows(sol2);

        List<int[]> test3 = Arrays.asList(
                new int[]{1, 10},
                new int[]{2, 6},
                new int[]{3, 5},
                new int[]{7, 9}
        );
        List<int[]> sol3 = mergeTimeWindows(test3);
        printTimeWindows(sol3);
    }

    public static List<int[]> mergeTimeWindows(List<int[]> input) {
        // sort input comparing first element
        input.sort(Comparator.comparing(ints -> ints[0]));

        List<int[]> result = new ArrayList<>();

        for (int[] slot : input) {
            if (result.isEmpty()) {
                result.add(slot);
            } else {
                int[] last = result.get(result.size() - 1);

                // need merging
                if (slot[0] <= last[1]) {
                    if (last[1] < slot[1]) {
                        last[1] = slot[1];
                    }
                } else {
                    result.add(slot);
                }
            }
        }

        return result;
    }

    public static void printTimeWindows(List<int[]> input) {
        for (int[] slot : input) {
            System.out.print(String.format("[%d,%d] ", slot[0], slot[1]));
        }
        System.out.println();
    }
}
