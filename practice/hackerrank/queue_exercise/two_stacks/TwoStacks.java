import java.util.Scanner;
import java.util.Stack;


public class TwoStacks {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        Stack<Long> inbox = new Stack<>();
        Stack<Long> outbox = new Stack<>();

        int q = scanner.nextInt();

        for (int i = 0; i < q; i++) {
            int t = scanner.nextInt();

            if (t == 1) {
                long x = scanner.nextLong();
                inbox.push(x);
            } else {
                if (outbox.isEmpty()) {
                    while (!inbox.isEmpty()) {
                        outbox.push(inbox.pop());
                    }
                }

                if (t == 2) {
                    outbox.pop();
                } else {
                    System.out.println(outbox.peek());
                }
            }
        }
    }

}
