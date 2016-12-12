import java.util.Scanner;
import java.util.Stack;

/**
 * Created by bmk on 12/12/16.
 */
public class MaximumElement {

    private static Scanner scanner = new Scanner(System.in);

    public static void main(String[] args) {
        int n = scanner.nextInt();
        Stack<Integer> stack = new Stack<>();

        for (int i = 0; i < n; i++) {
            int cmd = scanner.nextInt();

            switch (cmd) {
                case 1:
                    int value = scanner.nextInt();

                    if (stack.isEmpty()) {
                        stack.push(value);
                    } else {
                        if (value <= stack.peek()) {
                            stack.push(stack.peek());
                        } else {
                            stack.push(value);
                        }
                    }
                    break;
                case 2:
                    stack.pop();
                    break;
                case 3:
                    System.out.println(stack.peek());
            }
        }
    }
}
