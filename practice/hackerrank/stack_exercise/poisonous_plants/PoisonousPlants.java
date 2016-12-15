import java.util.*;

/**
 * Created by bmk on 12/15/16.
 */
public class PoisonousPlants {

    public static void main(String[] args) {
        /*Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();

        Stack<Integer> stack = new Stack<>();
        for (int i = 0; i < n; i++) {
            stack.push(scanner.nextInt());
        }

        System.out.println(bruteForce(stack));

        Collections.reverse(stack);

        long result = 0;
        long l;

        do {
            l = solve(stack);
            if (l > result) {
                result = l;
            }
        } while (l > 0);

        System.out.println(result);*/
        bugSearch();
    }

    public static void bugSearch() {
        Random r = new Random();

        while (true) {
            int[] input = new int[10];

            for (int i = 0; i < input.length; i++) {
                input[i] = r.nextInt(100);
            }

            Stack<Integer> stack = new Stack<>();
            for (int i : input) {
                stack.push(i);
            }

            long result1 = bruteForce(stack);

            Collections.reverse(stack);

            long result2 = 0;
            long l;

            do {
                l = solve(stack);
                if (l > result2) {
                    result2 = l;
                }
            } while (l > 0);

            if (result1 != result2) {
                for (int i : input) {
                    System.out.print(i + " ");
                }
                System.out.println();

                break;
            }
        }

    }

    public static long bruteForce(Stack<Integer> stack) {
        int counter = 0;
        boolean flag = true;

        List<Integer> list = stack.subList(0, stack.size());

        while (flag) {
            flag = false;
            List<Integer> newList = new ArrayList<>();

            for (int i = 0; i < list.size(); i++) {
                if (i == 0) {
                    newList.add(list.get(i));
                } else {
                    if (list.get(i - 1) < list.get(i)) {
                        flag = true;
                    } else {
                        newList.add(list.get(i));
                    }
                }
            }

            if (flag) {
                counter++;
                list = newList;
            }
        }

        return counter;
    }

    // broken
    public static long solve(Stack<Integer> stack) {
        long startValue = Long.MAX_VALUE;
        long day = 0;
        Stack<Entry> history = new Stack<>();

        try {
            // skip falling piece
            while (stack.peek() <= startValue) {
                startValue = stack.pop();
            }

            while (true) {
                long v = 0;
                long d = 1;

                // raising piece
                while (!stack.isEmpty() && stack.peek() > v) {
                    v = stack.pop();
                }

                if (d > day) {
                    day = d;
                }

                // falling piece
                while (stack.peek() > startValue && stack.peek() <= v) {
                    v = stack.pop();
                    d++;
                }

                while (!history.isEmpty() && history.peek().day <= d) {
                    history.pop();
                }

                while (!history.isEmpty() && history.peek().value >= v) {
                    d = history.pop().day + 1;
                }

                history.push(new Entry(d, v));

                if (d > day) {
                    day = d;
                }

                if (stack.peek() <= startValue) {
                    break;
                }
            }
        } catch (EmptyStackException e) {
            // do nothing!
        }

        return day;
    }

    public static class Entry {
        long day;
        long value;

        public Entry(long day, long value) {
            this.day = day;
            this.value = value;
        }
    }
}
