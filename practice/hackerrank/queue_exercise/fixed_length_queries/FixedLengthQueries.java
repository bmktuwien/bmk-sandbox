import java.util.ArrayList;
import java.util.Scanner;

/**
 * Created by bmk on 12/26/16.
 */
public class FixedLengthQueries {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        int n = scanner.nextInt();
        int q = scanner.nextInt();

        int[] input = new int[n];
        for (int i = 0; i < n; i++) {
            input[i] = scanner.nextInt();
        }

        for (int i = 0; i < q; i++) {
            int d = scanner.nextInt();
            solveQuery(d, input);
        }
    }

    public static void solveQuery(int d, int[] input) {
        ArrayList<Integer> maxs = new ArrayList<>();

        if (d == 1) {
            for (int i : input) {
                maxs.add(i);
            }
        } else {
            int[] tmp = new int[d - 1];

            int k = d - 1;
            while (k < input.length) {
                int max = -1;
                for (int i = 0; i < tmp.length; i++) {
                    if (input[k - i - 1] > max) {
                        max = input[k - i - 1];
                    }

                    tmp[i] = max;
                }


                max = -1;
                for (int i = 0; k + i < input.length && i < d - 1; i++) {
                    if (input[k + i] > max) {
                        max = input[k + i];
                    }

                    if (tmp[tmp.length - 1 - i] > max) {
                        maxs.add(tmp[tmp.length - 1 - i]);
                    } else {
                        maxs.add(max);
                    }
                }

                k += d - 1;
            }
        }


        int min = Integer.MAX_VALUE;
        for (int i : maxs) {
            if (i < min) {
                min = i;
            }
        }

        System.out.println(min);
    }
}
