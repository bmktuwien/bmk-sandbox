import java.util.Collections;
import java.util.Scanner;
import java.util.Stack;

public class EqualStacks {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        scanner.nextLine();

        Stack<Integer> stack1 = new Stack<>();
        Stack<Integer> stack2 = new Stack<>();
        Stack<Integer> stack3 = new Stack<>();

        long height1 = readStack(stack1, scanner.nextLine());
        long height2 = readStack(stack2, scanner.nextLine());
        long height3 = readStack(stack3, scanner.nextLine());

        while (!stack1.isEmpty() && !stack2.isEmpty() && !stack3.empty()) {
            if (height1 == height2 && height2 == height3) {
                System.out.println(height1 + "");
                return;
            } else {
                long l1 = height1 - stack1.peek();
                long l2 = height2 - stack2.peek();
                long l3 = height3 - stack3.peek();

                if (l1 >= l2 && l1 >= l3) {
                    height1 = l1;
                    stack1.pop();
                } else if (l2 >= l1 && l2 >= l3) {
                    height2 = l2;
                    stack2.pop();
                } else {
                    height3 = l3;
                    stack3.pop();
                }
            }
        }

        if (height1 == height2 && height2 == height3) {
            System.out.println(height1 + "");
        } else {
            System.out.println("0");
        }
    }

    public static long readStack(Stack<Integer> stack, String line) {
        long height = 0;
        Scanner s = new Scanner(line);

        while (s.hasNextInt()) {
            int h = s.nextInt();
            height += h;
            stack.push(h);
        }

        Collections.reverse(stack);

        return height;
    }
}
