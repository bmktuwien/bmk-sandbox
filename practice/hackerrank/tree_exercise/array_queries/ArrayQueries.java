import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;

public class ArrayQueries {

    static int chunkSize = 3;

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();
        int m = scanner.nextInt();

        Chunk head = null;
        Chunk prev = null;

        int i = 0;
        while (i < n) {
            long[] chunk = new long[chunkSize];

            int j = 0;
            int from = i;
            int to = i;
            while (i < n && j < chunk.length) {
                chunk[j] = scanner.nextInt();
                j++;
                i++;
                to++;
            }

            if (j > 0) {
                Chunk c = new Chunk(from, to - 1, chunk);

                if (head == null) {
                    head = c;
                    prev = head;
                } else {
                    prev.next = c;
                    prev = c;
                }
            }
        }

        System.out.println("done");
    }

    public static class Chunk {
        int from;
        int to;
        long[] content;
        Chunk next;

        public Chunk(int from, int to, long[] content) {
            this.from = from;
            this.to = to;
            this.content = content;
        }

        public Chunk splitA(int i) {
            if (i < from || to < i) {
                throw new RuntimeException();
            }

            Chunk c = new Chunk(i, to, Arrays.copyOfRange(content, i - from, to - from + 1));
            c.next = next;

            this.to = i - 1;
            this.content = Arrays.copyOfRange(content, 0, i - from);
            this.next = null;

            return c;
        }
    }
}
