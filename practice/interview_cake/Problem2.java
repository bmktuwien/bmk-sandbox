public class Problem2 {

    public static void main(String[] args) {
        int[] test1 = {1, 7, 3, 4};
        int[] sol1 = getProductsOfAllIntsExceptAtIndex(test1);
        printIntArray(sol1);

        int[] test2 = {5, 7, 0, 9};
        int[] sol2 = getProductsOfAllIntsExceptAtIndex(test2);
        printIntArray(sol2);

        int[] test3 = {1};
        int[] sol3 = getProductsOfAllIntsExceptAtIndex(test3);
        printIntArray(sol3);

        int[] test4 = {9, 2};
        int[] sol4 = getProductsOfAllIntsExceptAtIndex(test4);
        printIntArray(sol4);

        int[] test5 = {0, 9, 0};
        int[] sol5 = getProductsOfAllIntsExceptAtIndex(test5);
        printIntArray(sol5);
    }

    public static int[] getProductsOfAllIntsExceptAtIndex(int[] input) {
        int[] solution = new int[input.length];

        // 1st pass
        for (int i = input.length - 1; i >= 0; i--) {
            if (i == input.length - 1) {
                solution[i] = input[i];
            } else {
                solution[i] = input[i] * solution[i+1];
            }
        }

        // 2nd pass
        int tmp = 1;
        for (int i = 0; i < input.length; i++) {
            if (i < input.length - 1) {
                solution[i] = tmp * solution[i+1];
                tmp *= input[i];
            } else {
                solution[i] = tmp;
            }
        }

        return solution;
    }

    public static void printIntArray(int[] array) {
        for (int i : array) {
            System.out.print(i + " ");
        }
        System.out.println();
    }

}
