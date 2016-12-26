import java.util.LinkedList;
import java.util.Queue;
import java.util.Scanner;

/**
 * Created by bmk on 12/26/16.
 */
public class TruckTour {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        int n = scanner.nextInt();

        Pump[] pumps = new Pump[n];
        for (int i = 0; i < n; i++) {
            Pump p = new Pump();
            p.petrol = scanner.nextInt();
            p.next = scanner.nextInt();
            pumps[i] = p;
        }

        Queue<Entry> queue = new LinkedList<>();
        boolean[] tourStartedAt = new boolean[n];

        queue.add(new Entry(pumps[0].petrol, 0, 0));
        tourStartedAt[0] = true;

        while (!queue.isEmpty()) {
            Entry e = queue.remove();

            int nextPos = (e.currentPos + 1) % n;

            if (nextPos == e.startedAt) {
                System.out.println(e.startedAt);
                break;
            }

            if (e.capacity >= pumps[e.currentPos].next) {
                Entry eNew = new Entry(e.capacity - pumps[e.currentPos].next + pumps[nextPos].petrol, e.startedAt, nextPos);
                queue.add(eNew);
            }

            if (!tourStartedAt[nextPos]) {
                tourStartedAt[nextPos] = true;
                queue.add(new Entry(pumps[nextPos].petrol, nextPos, nextPos));
            }
        }
    }

    public static class Pump {
        int petrol;
        int next;
    }

    public static class Entry {
        int capacity;
        int startedAt;
        int currentPos;

        public Entry(int capacity, int startedAt, int currentPos) {
            this.capacity = capacity;
            this.startedAt = startedAt;
            this.currentPos = currentPos;
        }
    }
}
