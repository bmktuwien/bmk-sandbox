

public class ArrayQueries {

    public static void main(String[] args) {
        int k = 0;
        for (int i = 0; i < 10000; i++) {
            for (int j = 0; j < 10000; j++ ) {
                k += i + j;
            }
        }

        System.out.println(k);
    }
}
