import java.util.*;

/**
 * Created by bmk on 12/9/16.
 */
public class SquareTenTree {

    private static Scanner scanner = new Scanner(System.in);

    public static void main(String[] args) {
        int[] lDigits = readDigits();
        int[] rDigits = readDigits();

        lDigits = Arrays.copyOfRange(lDigits, 0, rDigits.length);

        findMinDecomposition(lDigits, rDigits);
    }

    public static void findMinDecomposition(int[] lDigits, int[] rDigits) {
        int[] lLevels = new int[lDigits.length];
        int[] rLevels = new int[rDigits.length];

        int k = -1;
        for (int i = lDigits.length - 1; i > 0; i--) {
            if (lDigits[i] < rDigits[i]) {
                k = i;
                break;
            }
        }

        int c = 0;
        if (k > 0) {
            if (lDigits[0] > 1) {
                lLevels[0] = 11 - lDigits[0];
                c = 1;
            } else {
                lLevels[0] = 1 - lDigits[0];
            }

            if (rDigits[0] != 0) {
                rLevels[0] =  rDigits[0];
            }
        } else {
            lLevels[0] =  rDigits[0] - lDigits[0] + 1;
        }

        int level = 1;
        long factor = 1;

        for (int i = 1; i <= k; i++) {
            if (i == k) {
                lLevels[level] += (rDigits[i] - lDigits[i] - c) * factor;
            } else {
                if (lDigits[i] + c != 0) {
                    lLevels[level] +=  (10 - lDigits[i] - c) * factor;
                    c = 1;
                }

                if (rDigits[i] != 0) {
                    rLevels[level] += rDigits[i] * factor;
                }
            }

            // if next index is power of 2, then reset factor and increase level
            if (isPowerOfTwo(i + 1)) {
                level++;
                factor = 1;
            } else {
                factor *= 10;
            }
        }

        // post processing
        // merge
        for (int i = lLevels.length - 1; i >= 0; i--) {
            if (rLevels[i] != 0 || lLevels[i] != 0) {
                if (rLevels[i] != 0 && lLevels[i] != 0) {
                    lLevels[i] += rLevels[i];
                    rLevels[i] = 0;
                }

                break;
            }
        }

        int counter = 0;
        for (int i = 0; i < lLevels.length; i++) {
            if (lLevels[i] != 0) {
                counter++;
            }

            if (rLevels[i] != 0) {
                counter++;
            }
        }

        System.out.println(counter);

        for (int i = 0; i < lLevels.length; i++) {
            if (lLevels[i] != 0) {
                System.out.println(i + " " + lLevels[i]);
            }
        }

        for (int i = rLevels.length - 1; i >= 0; i--) {
            if (rLevels[i] != 0) {
                System.out.println(i + " " + rLevels[i]);
            }
        }
    }

    public static int[] readDigits() {
        String line = scanner.nextLine();

        ArrayList<Integer> digits = new ArrayList<>();

        for (int i = 0; i < line.length(); i++) {
            digits.add(line.charAt(i) - '0');
        }

        Collections.reverse(digits);

        return digits.stream().mapToInt(i->i).toArray();
    }

    public static boolean isPowerOfTwo(int x) {
        return (x & (x - 1)) == 0;
    }
}
