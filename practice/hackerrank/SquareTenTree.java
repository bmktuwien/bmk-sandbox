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
        StringBuffer[] lLevels = new StringBuffer[lDigits.length];
        StringBuffer[] rLevels = new StringBuffer[rDigits.length];

        int k = -1;
        for (int i = lDigits.length - 1; i > 0; i--) {
            if (lDigits[i] < rDigits[i]) {
                k = i;
                break;
            }
        }

        // initialize
        for (int i = 0; i < lLevels.length; i++) {
            lLevels[i] = new StringBuffer();
            rLevels[i] = new StringBuffer();
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
        for (int i = 1; i <= k; i++) {
            if (i == k) {
                if (rDigits[i] - lDigits[i] - c != 0) {
                    lLevels[level].append((rDigits[i] - lDigits[i] - c));
                }
            } else {
                lLevels[level].append((10 - lDigits[i] - c) % 10);
                rLevels[level].append(rDigits[i]);

                if (lDigits[i] + c != 0) {
                    c = 1;
                }
            }

            // if next index is power of 2, then reset factor and increase level
            if (isPowerOfTwo(i + 1)) {
                level++;
            }
        }

        // post processing
        boolean merged = false;
        for (int i = level; i >= 0; i--) {
            lLevels[i].reverse();
            rLevels[i].reverse();

            if (!merged && (!isZero(rLevels[i]) || !isZero(lLevels[i]))) {
                if (!isZero(rLevels[i]) && !isZero(lLevels[i])) {
                    BigInteger b1 = new BigInteger(lLevels[i].toString());
                    BigInteger b2 = new BigInteger(rLevels[i].toString());

                    lLevels[i] = new StringBuffer(b1.add(b2).toString());
                    rLevels[i] = new StringBuffer();
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

    public static boolean isZero(StringBuffer s) {
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) != '0') {
                return false;
            }
        }

        return true;
    }

}
