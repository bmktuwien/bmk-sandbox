import java.util.Arrays;

/**
 * Created by bmk on 12/6/16.
 */
public class Problem3 {

    public static void main(String[] args) {
        int[] test = {1, 7, 3, 4};
        int sol = maxProductOfThreeInts(test);
        System.out.println(sol);

        int[] test2 = {1, 7, 3, 4, 7, 7};
        int sol2 = maxProductOfThreeInts(test2);
        System.out.println(sol2);

        int[] test3 = {1, 7, 3, 4, -7, -7};
        int sol3 = maxProductOfThreeInts(test3);
        System.out.println(sol3);

        int[] test4 = {1, 7, 3, 4, 7, -7};
        int sol4 = maxProductOfThreeInts(test4);
        System.out.println(sol4);

        int[] test5 = {-1, 0, -3, -4, -7, -7};
        int sol5 = maxProductOfThreeInts(test5);
        System.out.println(sol5);

        int[] test6 = {-1, 1, -3, -6, -7, -7};
        int sol6 = maxProductOfThreeInts(test6);
        System.out.println(sol6);
    }

    public static int maxProductOfThreeInts(int[] input) {
        if (input.length < 3) {
            throw new IllegalArgumentException();
        }

        int[] max = Arrays.copyOfRange(input, 0, 3);
        int[] min = Arrays.copyOfRange(input, 0, 2);


        for (int i = 2; i < input.length; i++) {
            //update max
            if (i > 2) {
                int j = max[0] < max[1] ? 0 : 1;
                j = max[j] < max[2] ? j : 2;

                if (max[j] < input[i]) {
                    max[j] = input[i];
                }
            }

            //update min
            int j = min[0] > min[1] ? 0 : 1;
            if (min[j] > input[i]) {
                min[j] = input[i];
            }
        }

        int candidate1 = max[0] * max[1] * max[2];
        int candidate2 = Math.max(Math.max(max[0], max[1]), max[2]) * min[0] * min[1];

        return Math.max(candidate1, candidate2);
    }
}
