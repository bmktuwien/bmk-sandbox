import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.Stack;

public class Waiter {

    public static void main(String[] args) {
        List<Integer> primes = generatePrimeNumbers();

        Scanner s = new Scanner(System.in);
        int n = s.nextInt();
        int q = s.nextInt();

        Stack<Integer> stack = new Stack<>();
        for (int i = 0; i < n; i++) {
            stack.push(s.nextInt());
        }

        solve(q, stack, primes);
    }

    public static void solve(int q, Stack<Integer> stack, List<Integer> primes) {
        for (int i = 0; i < q; i++) {
            int p = primes.get(i);
            Stack<Integer> newStack = new Stack<>();
            Stack<Integer> print = new Stack<>();

            while (!stack.isEmpty()) {
                int j = stack.pop();
                if (j % p == 0) {
                    print.push(j);
                } else {
                    newStack.push(j);
                }
            }

            while (!print.isEmpty()) {
                System.out.println(print.pop());
            }

            stack = newStack;
        }

        while (!stack.isEmpty()) {
            System.out.println(stack.pop());
        }
    }

    public static List<Integer> generatePrimeNumbers() {
        boolean[] notPrime = new boolean[10000];

        for (int i = 2; i < notPrime.length; i++) {
            if (!notPrime[i]) {
                for (int j = 2; i * j < notPrime.length; j++) {
                    notPrime[i*j] = true;
                }
            }
        }

        List<Integer> primes = new ArrayList<>();
        for (int i = 2; i < notPrime.length; i++) {
            if (!notPrime[i]) {
                primes.add(i);
            }
        }

        return primes;
    }
}
