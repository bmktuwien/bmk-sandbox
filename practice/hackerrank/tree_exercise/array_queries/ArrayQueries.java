import java.util.Arrays;
import java.util.Scanner;

public class ArrayQueries {

    static Scanner scanner = new Scanner(System.in);
    static int chunkSize = 3;

    public static void main(String[] args) {
        int n = scanner.nextInt();
        int m = scanner.nextInt();

        Chunk head = readInput(n);

        head = prependAtFront(4, 5, head);
        System.out.println("done");
    }

    public static Chunk readInput(int n) {
        Chunk head = null;
        Chunk prev = null;

        int i = 0;
        while (i < n) {
            long[] chunk = new long[chunkSize];

            int j = 0;
            while (i < n && j < chunk.length) {
                chunk[j] = scanner.nextInt();
                j++;
                i++;
            }

            if (j > 0) {
                Chunk c = new Chunk(chunk);

                if (head == null) {
                    head = c;
                    prev = head;
                } else {
                    prev.next = c;
                    prev = c;
                }
            }
        }

        return head;
    }

    public static Chunk prependAtFront(int i, int j, Chunk head) {
        ChunkPair cp = findInterval(i, j, head);

        if (cp.ch1 != cp.ch2) {
            Chunk newHead = cp.ch1.split(i - cp.beginIdx1);
            cp.ch1.next = cp.ch2.split(j - cp.beginIdx2 + 1);
            cp.ch2.next = head;

            return newHead;
        } else {
            Chunk newHead = cp.ch1.split2(i - cp.beginIdx1, j - cp.beginIdx1);
            newHead.next = head;

            return newHead;
        }
    }

    public static ChunkPair findInterval(int i, int j, Chunk head) {
        int idx = 0;
        Chunk ch1 = null;
        Chunk ch2 = null;
        int beginIdx1 = 0;
        int beginIdx2 = 0;

        while (head != null) {
            int from = idx;
            int to = from + head.content.length - 1;

            if (i >= from && i <= to) {
                ch1 = head;
                beginIdx1 = from;
            }

            if (j >= from && j <= to) {
                ch2 = head;
                beginIdx2 = from;
                break;
            }

            idx = from + head.content.length;
            head = head.next;
        }

        return new ChunkPair(ch1, ch2, beginIdx1, beginIdx2);
    }

    public static class ChunkPair {
        int beginIdx1;
        int beginIdx2;
        Chunk ch1;
        Chunk ch2;

        public ChunkPair(Chunk ch1, Chunk ch2, int beginIdx1, int beginIdx2) {
            this.ch1 = ch1;
            this.ch2 = ch2;
            this.beginIdx1 = beginIdx1;
            this.beginIdx2 = beginIdx2;
        }
    }

    public static class Chunk {
        long[] content;
        Chunk next;

        public Chunk(long[] content) {
            this.content = content;
        }

        public Chunk split(int i) {
            Chunk c = new Chunk(Arrays.copyOfRange(content, i, content.length));
            c.next = next;

            this.content = Arrays.copyOfRange(content, 0, i);
            this.next = null;

            return c;
        }

        public Chunk split2(int i, int j) {
            long[] ls1 = Arrays.copyOfRange(this.content, i, j + 1);
            long[] ls2 = Arrays.copyOfRange(this.content, 0, i);
            long[] ls3 = Arrays.copyOfRange(this.content, j + 1, this.content.length);

            this.content = concat(ls2, ls3);

            return new Chunk(ls1);

        }
    }

    public static long[] concat(long[] a, long[] b) {
        int aLen = a.length;
        int bLen = b.length;
        long[] c = new long[aLen+bLen];
        System.arraycopy(a, 0, c, 0, aLen);
        System.arraycopy(b, 0, c, aLen, bLen);

        return c;
    }
}
