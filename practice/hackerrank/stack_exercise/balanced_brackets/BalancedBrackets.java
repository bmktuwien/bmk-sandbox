import java.util.EmptyStackException;
import java.util.ListIterator;
import java.util.Scanner;
import java.util.Stack;

/**
 * Created by bmk on 12/12/16.
 */
public class BalancedBrackets {

    private static Scanner scanner = new Scanner(System.in);

    public static void main(String[] args) {
        int n = scanner.nextInt();
        scanner.nextLine();


        for (int i = 0; i < n; i++) {
            String line = scanner.nextLine();

            if (balanced(line)) {
                System.out.println("YES");
            } else {
                System.out.println("NO");
            }
        }
    }

    public static boolean balanced(String line) {
        Stack<Character> stack = new Stack<>();
        Boolean flag = null;

        try {
            for (char c : line.toCharArray()) {
                Character matchingOpenChr = null;

                switch (c) {
                    case '(':
                    case '{':
                    case '[':
                        stack.push(c);
                        continue;
                    case ')':
                        matchingOpenChr = '(';
                        break;
                    case '}':
                        matchingOpenChr = '{';
                        break;
                    case ']':
                        matchingOpenChr = '[';
                        break;
                }

                if (stack.pop() != matchingOpenChr) {
                    flag = false;
                } else {
                    if (flag != null && !flag) {
                        return false;
                    }

                    flag = true;
                }
            }

            return stack.isEmpty();
        } catch (EmptyStackException e) {
            return false;
        }
    }
}
