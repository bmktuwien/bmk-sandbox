import java.util.TreeMap;

public class MedianUpdates {

    public static void main(String[] args) {

        TreeMap<Integer, Integer> map = new TreeMap<>();

        int median = -1;

        for (int i = 0; i < 10; i++) {
            map.put(i, i);

            if (median == -1) {
                median = i;
            } else {
                median = map.higherKey(median);
            }

            System.out.println(median);
        }
    }
}
