import java.util.*;

/**
 * Created by bmk on 12/15/16.
 */
public class PoisonousPlants {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();

        LinkedList<Integer> list = new LinkedList<>();
        for (int i = 0; i < n; i++) {
            list.add(scanner.nextInt());
        }

        long result = solve(list);
        System.out.println(result);
    }

    public static void bugSearch() {
        Random r = new Random();

        while (true) {
            int[] input = new int[20];

            for (int i = 0; i < input.length; i++) {
                input[i] = r.nextInt(100);
            }

            LinkedList<Integer> list = new LinkedList<>();
            for (int i : input) {
                list.add(i);
            }

            long result1 = bruteForce(list);
            long result2 = solve(list);

            if (result1 != result2) {
                for (int i : input) {
                    System.out.print(i + " ");
                }
                System.out.println();

                break;
            }
        }
    }

    public static long bruteForce(LinkedList<Integer> list) {
        int counter = 0;
        boolean flag = true;

        while (flag) {
            flag = false;
            LinkedList<Integer> newList = new LinkedList<>();

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

    public static long solve(LinkedList<Integer> list) {
        long result = 0;
        long l;

        do {
            l = solveHelper(list);
            if (l > result) {
                result = l;
            }
        } while (l > 0);

        return result;
    }

    public static long solveHelper(LinkedList<Integer> list) {
        long startValue = Long.MAX_VALUE;
        long day = 0;
        Stack<Entry> history = new Stack<>();

        try {
            // skip falling piece
            while (list.peek() <= startValue) {
                startValue = list.remove();
            }

            while (true) {
                long v = 0;
                long d = 1;

                // raising piece
                while (!list.isEmpty() && list.peek() > v) {
                    v = list.remove();
                }

                if (d > day) {
                    day = d;
                }

                // falling piece
                while (!list.isEmpty() && list.peek() > startValue && list.peek() <= v) {
                    v = list.remove();
                    d++;

                    while (!history.isEmpty() && history.peek().day < d) {
                        history.pop();
                    }

                    while (!history.isEmpty() && history.peek().value >= v) {
                        d = history.pop().day + 1;
                    }

                    if (!history.isEmpty() && history.peek().day <= d) {
                        history.pop();
                    }

                    history.push(new Entry(d, v));
                }

                if (d > day) {
                    day = d;
                }

                if (list.peek() <= startValue) {
                    break;
                }
            }
        } catch (NoSuchElementException | NullPointerException e) {
            // do nothing!
        }

        return day;
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    public static class Entry {
        long day;
        long value;

        public Entry(long day, long value) {
            this.day = day;
            this.value = value;
        }
    }
}
