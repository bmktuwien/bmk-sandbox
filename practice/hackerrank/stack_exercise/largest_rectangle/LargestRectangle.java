import java.util.Scanner;
import java.util.Stack;

/**
 * Created by bmk on 12/13/16.
 */
public class LargestRectangle {

    public static void main(String[] args) {
        long[] heights = readInput();
        long result = 0;
        Stack<Entry> stack = new Stack<>();

        for (long h : heights) {
            if (stack.isEmpty()) {
                stack.push(new Entry(h));
            } else {
                if (stack.peek().height < h) {
                    stack.push(new Entry(h));
                } else if (stack.peek().height == h) {
                    stack.peek().counter += 1;
                } else {

                    int counter = 0;
                    while (!stack.isEmpty() && stack.peek().height > h) {
                        Entry e = stack.pop();
                        counter += e.counter;

                        if (e.height * counter > result) {
                            result = e.height * counter;
                        }
                    }

                    if (!stack.isEmpty()) {
                        if (h == stack.peek().height) {
                            stack.peek().counter += counter + 1;
                        } else if (h > stack.peek().height) {
                            stack.push(new Entry(h, counter + 1));
                        } else {
                            throw new RuntimeException("Impossible happened");
                        }
                    } else {
                        stack.push(new Entry(h, counter + 1));
                    }
                }
            }
        }

        int counter = 0;
        while (!stack.isEmpty()) {
            Entry e = stack.pop();
            counter += e.counter;

            if (e.height * counter > result) {
                result = e.height * counter;
            }
        }

        System.out.println(result);
    }

    public static long[] readInput() {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();

        long[] heights = new long[n];
        for (int i = 0; i < heights.length; i++) {
            heights[i] = scanner.nextLong();
        }

        return heights;
    }

    public static class Entry {
        long height;
        int counter;

        public Entry(long height) {
            this.height = height;
            counter = 1;
        }

        public Entry(long height, int counter) {
            this.height = height;
            this.counter = counter;
        }
    }
}
