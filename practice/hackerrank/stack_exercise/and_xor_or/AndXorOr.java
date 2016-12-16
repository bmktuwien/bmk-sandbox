import java.util.Arrays;
import java.util.Random;
import java.util.Scanner;

/**
 * Created by bmk on 12/16/16.
 */
public class AndXorOr {

    public static void main(String[] args) {
        long[] input = getRandomInput(1000000);
        System.out.println(bruteForce(input));
        //experiment3();
    }

    public static long evaluate(long l1, long l2) {
        return ((l1 & l2) ^ (l1 | l2)) & (l1 ^ l2);
    }

    public static long bruteForce(long[] input) {
        long max = 0;

        for (int i = 0; i < input.length - 1; i++) {
            long min = input[i + 1];
            for (int j = i + 1; j < input.length; j++) {
                if (input[j] <= min) {
                    min = input[j];
                    long l = evaluate(input[i], input[j]);

                    if (l > max) {
                        max = l;
                    }
                }
            }
        }

        return max;
    }

    public static void experiment3() {
        Random r = new Random();

        long[] input = new long[1000000];
        for (int i = 0; i < input.length; i++) {
            input[i] = r.nextInt(20);
            System.out.print(input[i] + " ");
        }

        System.out.println();

        for (int i = 1; i < input.length; i++) {
            long[] sub = Arrays.copyOf(input, i);
            System.out.println(bruteForce(sub));
        }
    }

    public static long[] getRandomInput(int n) {
        Random r = new Random();

        long[] input = new long[n];
        for (int i = 0; i < input.length; i++) {
            input[i] = r.nextInt(20);
            //System.out.print(input[i] + " ");
        }

        return input;
    }

    public static void experiment() {
        for (int k = 0; k < 100; k ++) {
            long max = 0;
            long tmp = 0;

            for (int i = 0; i < 300; i++) {
                long l = evaluate(k, i);

                if (l > max) {
                    max = l;
                    System.out.print(String.format("%5d:%-5d ", (i - tmp), max));
                    tmp = i;
                }
            }

            System.out.println();
        }
    }

    public static void experiment2() {
        for (int k = 0; k < 100; k++) {
            for (int i =  k; i < 100; i++) {
                long l = evaluate(k, i);

                System.out.print(String.format("%4d ", l));
            }

            System.out.println();
        }
    }

    public static long[] readInput() {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();
        long[] input = new long[n];

        for (int i = 0; i < n; i++) {
            input[i] = scanner.nextLong();
        }

        return input;
    }
}
