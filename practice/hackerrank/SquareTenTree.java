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
                lLevels[0] = 10 - lDigits[0];
                c = 1;
            } else {
                lLevels[0] = 1 - lDigits[0];
            }

            rLevels[0] =  rDigits[0];
        } else {
            lLevels[0] =  rDigits[0] - lDigits[0] + 1;
        }

        for (int i = 1; i <= k; i++) {
            if (i == k) {
                lLevels[i] = rDigits[i] - lDigits[i] - c;
            } else {
                if (lDigits[i] + c != 0) {
                    lLevels[i] =  10 - lDigits[i] - c;
                    c = 1;
                }

                if (rDigits[i] != 0) {
                    rLevels[i] = rDigits[i];
                }
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
}
