import java.util.ArrayList;
import java.util.Scanner;

/**
 * Created by bmk on 12/26/16.
 */
public class FixedLengthQueries {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        int n = scanner.nextInt();
        int d = scanner.nextInt();

        int[] input = new int[n];
        for (int i = 0; i < n; i++) {
            input[i] = scanner.nextInt();
        }

        solveQuery(d, input);
    }

    public static void solveQuery(int d, int[] input) {
        ArrayList<Integer> result = new ArrayList<>();
        int[] tmp = new int[d-1];

        int k = d-1;
        while (k < input.length) {
            int max = -1;
            for (int i = 0; i < tmp.length; i++) {
                if (input[k-i-1] > max) {
                    max = input[k-i-1];
                }

                tmp[i] = max;
            }


            max = -1;
            for (int i = 0; k+i < input.length && i < d-1; i++) {
                if (input[k+i] > max) {
                    max = input[k+i];
                }

                if (tmp[tmp.length-1-i] > max) {
                    result.add(tmp[tmp.length-1-i]);
                } else {
                    result.add(max);
                }
            }

            k += d-1;
        }

        System.out.println("done");
    }
}
