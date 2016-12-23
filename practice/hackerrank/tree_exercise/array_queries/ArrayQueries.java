import java.util.Arrays;
import java.util.Random;
import java.util.Scanner;

public class ArrayQueries {

    static Scanner scanner = new Scanner(System.in);
    static int chunkSize = 1000;

    public static void main(String[] args) {
        int n = scanner.nextInt();
        int m = scanner.nextInt();

        Chunk head = readInput(n);

        for (int i = 0; i < m; i++) {
            int type = scanner.nextInt();
            int p = scanner.nextInt();
            int q = scanner.nextInt();

            if (type == 1) {
                head = prepend(p - 1, q - 1, head);
            } else {
                head = append(p - 1, q - 1, head);
            }

            balance(head);
        }

        Chunk end = head;
        while (end.next != null) {
            end = end.next;
        }

        System.out.println(Math.abs(head.content[0] - end.content[end.content.length - 1]));
        while (head != null) {
            long[] content = head.content;
            for (long l : content) {
                System.out.print(l + " ");
            }

            head = head.next;
        }
        System.out.println();
    }

    public static void experiment() {
        Chunk head = randomInput(100000);

        Random r = new Random();
        for (int i = 0; i < 100000; i++) {
            int k = r.nextInt(10000);
            int l = Math.min(10000 - 1, k + r.nextInt(1000));

            head = append(k, l, head);

            balance(head);
        }
    }

    public static Chunk randomInput(int n) {
        Random r = new Random();
        Chunk head = null;
        Chunk prev = null;

        int i = 0;
        while (i < n) {
            long[] chunk = new long[Math.min(chunkSize, n - i)];

            int j = 0;
            while (i < n && j < chunk.length) {
                chunk[j] = r.nextInt(100000);
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

    public static Chunk readInput(int n) {
        Chunk head = null;
        Chunk prev = null;

        int i = 0;
        while (i < n) {
            long[] chunk = new long[Math.min(chunkSize, n - i)];

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

    public static Chunk prepend(int i, int j, Chunk head) {
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

    public static Chunk append(int i, int j, Chunk head) {
        ChunkPair cp = findInterval(i, j, head);


        if (cp.ch1 != cp.ch2) {
            Chunk c = cp.ch1.split(i - cp.beginIdx1);
            cp.ch1.next = cp.ch2.split(j - cp.beginIdx2 + 1);

            Chunk end = head;
            while (end.next != null) {
                end = end.next;
            }

            end.next = c;

            return head;
        } else {
            Chunk end = head;
            while (end.next != null) {
                end = end.next;
            }

            end.next = cp.ch1.split2(i - cp.beginIdx1, j - cp.beginIdx1);

            return head;
        }
    }

    public static void balance(Chunk head) {
        while (head != null) {
            if (head.getSize() >= 2 * chunkSize) {
                head.divide();
            } else if (head.getSize() < chunkSize / 2) {
                head.merge();
            }

            head = head.next;
        }
    }

    public static void printChunks(Chunk head) {
        int idx = 0;
        while (head != null) {
            int from = idx;
            int to = from + head.content.length - 1;

            System.out.print("Chunk: " + from + " - " + to + ", ");
            idx = from + head.content.length;
            head = head.next;
        }

        System.out.println();
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

        public int getSize() {
            return content.length;
        }

        public void divide() {
            int l = getSize() / 2;

            this.next = split(l);
        }

        public void merge() {
            if (next != null) {
                this.content = concat(this.content, next.content);
                this.next = this.next.next;
            }
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
