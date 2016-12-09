import java.util.ArrayList;
import java.util.Collections;
import java.util.Scanner;

/**
 * Created by bmk on 12/9/16.
 */
public class SquareTenTree {

    private static Scanner scanner = new Scanner(System.in);

    public static void main(String[] args) {
        int[] lDigits = readDigits();
        int[] rDigits = readDigits();

        for (int d : lDigits) {
            System.out.println(d);
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
