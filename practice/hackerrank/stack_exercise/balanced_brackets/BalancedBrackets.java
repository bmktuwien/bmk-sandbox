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

        try {
            for (char c : line.toCharArray()) {
                switch (c) {
                    case '(':
                    case '{':
                    case '[':
                        stack.push(c);
                        break;
                    case ')':
                        if (stack.pop() != '(') {
                            return false;
                        }
                        break;
                    case '}':
                        if (stack.pop() != '{') {
                            return false;
                        }
                        break;
                    case ']':
                        if (stack.pop() != '[') {
                            return false;
                        }
                        break;

                }
            }

            return true;
        } catch (EmptyStackException e) {
            return false;
        }
    }
}
