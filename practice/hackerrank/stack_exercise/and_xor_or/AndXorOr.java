import java.util.Arrays;
import java.util.Random;
import java.util.Scanner;

/**
 * Created by bmk on 12/16/16.
 */
public class AndXorOr {

    public static void main(String[] args) {
        //long[] input = getRandomInput(1000000);
        long[] input = readInput();
        //System.out.println(bruteForce(input));
        System.out.println(solve(input));
        //experiment3();
    }

    public static long evaluate(long l1, long l2) {
        return ((l1 & l2) ^ (l1 | l2)) & (l1 ^ l2);
    }

    public static long bruteForce(long[] input) {
        long max = 0;

        for (int i = 0; i < input.length - 1; i++) {
            long min = Long.MAX_VALUE;

            for (int j = i + 1; j < input.length; j++) {
                if (input[j] < min) {
                    min = input[j];
                    long l = evaluate(input[i], input[j]);

                    if (l > max) {
                        max = l;
                    }
                }

                if (min <= input[i]) {
                    break;
                }
            }
        }

        return max;
    }

    public static long solve(long[] input) {
        long max = 0;
        int firstPeak = 0;

        for (int i = 0; i < input.length - 1; i++) {
            int j = i + 1;

            long min = input[j];
            long l = evaluate(input[i], input[j]);

            if (l > max) {
                max = l;
            }

            boolean raising = true;
            long tmp = input[j];
            int firstPeakTmp = 0;

            j = Math.max(j, firstPeak);
            while (j < input.length) {

                // update first peak if i is past old firstPeak value
                if (raising && input[j] >= tmp) {
                    firstPeakTmp = j;
                    tmp = input[j];
                } else {
                    raising = false;
                }

                if (input[j] < min) {
                    min = input[j];
                    l = evaluate(input[i], input[j]);

                    if (l > max) {
                        max = l;
                    }
                }

                if (min <= input[i]) {
                    break;
                }

                j++;
            }

            if (i + 1 >= firstPeak) {
                firstPeak = firstPeakTmp;
            }
        }

        return max;
    }

    public static void experiment3() {
       while (true) {
           long[] input = getRandomInput(50);

           long r1 = bruteForce(input);
           long r2 = solve(input);

           if (r1 != r2) {
               for (long l : input) {
                   System.out.print(l + " ");
               }
               System.out.println();
               System.out.println(r1 + " " + r2);

               break;
           }
       }
    }

    public static long[] getRandomInput(int n) {
        Random r = new Random();

        long[] input = new long[n];
        for (int i = 0; i < input.length; i++) {
            input[i] = r.nextInt(30);
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
