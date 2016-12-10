import java.math.BigInteger;
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
        //TODO: calculate correct level!!!
        StringBuilder[] lLevels = new StringBuilder[lDigits.length];
        StringBuilder[] rLevels = new StringBuilder[rDigits.length];

        int k = -1;
        for (int i = lDigits.length - 1; i > 0; i--) {
            if (lDigits[i] < rDigits[i]) {
                k = i;
                break;
            }
        }

        // initialize
        for (int i = 0; i < lLevels.length; i++) {
            lLevels[i] = new StringBuilder();
            rLevels[i] = new StringBuilder();
        }

        int c = 0;
        if (k > 0) {
            if (lDigits[0] > 1) {
                lLevels[0].append(11 - lDigits[0]);
                c = 1;
            } else if (lDigits[0] == 0){
                lLevels[0].append(1);
            }

            rLevels[0].append(rDigits[0]);
        } else {
            lLevels[0].append(rDigits[0] - lDigits[0] + 1);
        }

        int level = 1;
        int lTemp = 0;
        int rTemp = 0;

        for (int i = 1; i <= k; i++) {
            if (i == k) {
                if (rDigits[i] - lDigits[i] - c != 0) {
                    lLevels[level].append((rDigits[i] - lDigits[i] - c));
                    lTemp = lLevels[level].length();
                }
            } else {
                lLevels[level].append((10 - lDigits[i] - c) % 10);
                rLevels[level].append(rDigits[i]);

                if ((10 - lDigits[i] - c) % 10 != 0) {
                    lTemp = lLevels[level].length();
                }

                if (rDigits[i] != 0) {
                    rTemp = rLevels[level].length();
                }

                if (lDigits[i] + c != 0) {
                    c = 1;
                }
            }

            // if next index is power of 2, then reset factor and increase level
            if (isPowerOfTwo(i + 1)) {
                lLevels[level] = new StringBuilder(lLevels[level].substring(0, lTemp));
                rLevels[level] = new StringBuilder(rLevels[level].substring(0, rTemp));

                lTemp = 0;
                rTemp = 0;

                if (i + 1 <= k) {
                    level++;
                }
            } else if (i == k) {
                lLevels[level] = new StringBuilder(lLevels[level].substring(0, lTemp));
                rLevels[level] = new StringBuilder(rLevels[level].substring(0, rTemp));
            }
        }

        // post processing
        boolean merged = false;
        for (int i = level; i >= 0; i--) {
            lLevels[i].reverse();
            rLevels[i].reverse();

            if (!merged && (!isZero(rLevels[i]) || !isZero(lLevels[i]))) {
                if (!isZero(rLevels[i]) && !isZero(lLevels[i])) {
                    lLevels[i] = add(lLevels[i], rLevels[i]);
                    rLevels[i] = new StringBuilder();
                }

                merged = true;
            }
        }

        int counter = 0;
        for (int i = 0; i <= level; i++) {
            if (!isZero(lLevels[i])) {
                counter++;
            }

            if (!isZero(rLevels[i])) {
                counter++;
            }
        }

        System.out.println(counter);

        for (int i = 0; i <= level; i++) {
            if (!isZero(lLevels[i])) {
                System.out.println(i + " " + lLevels[i]);
            }
        }

        for (int i = level; i >= 0; i--) {
            if (!isZero(rLevels[i])) {
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

    public static boolean isZero(StringBuilder s) {
        return s.length() == 0 || s.charAt(0) == '0';

    }

    public static StringBuilder add(StringBuilder s1, StringBuilder s2) {
        int i = s1.length() - 1;
        int j = s2.length() - 1;

        StringBuilder result = new StringBuilder();

        int c = 0;
        while (i >= 0 || j >= 0) {
            int d1 = i >= 0 ? s1.charAt(i) - '0' : 0;
            int d2 = j >= 0 ? s2.charAt(j) - '0' : 0;
            int s = d1 + d2 + c;

            result.append(s % 10);
            c = s / 10;

            i--;
            j--;
        }

        if (c != 0) {
            result.append(c);
        }

        return result.reverse();
    }
}
