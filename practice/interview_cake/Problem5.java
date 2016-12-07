import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by bmk on 12/7/16.
 */
public class Problem5 {

    public static void main(String[] args) {
        List<List<Integer>> sol1 = solve(4, new int[]{1, 2, 3, 4});
        prinSolution(sol1);
    }

    public static List<List<Integer>> solve(int amount, int[] denominations) {
        if (amount >= 0) {
            List<List<Integer>> result = new ArrayList<>();

            for (int d : denominations) {
                if (amount > d) {
                    List<List<Integer>> subResult = solve(amount - d, denominations);

                    for (List<Integer> l : subResult) {
                        if (l.get(l.size() - 1) <= d) {
                            l.add(d);
                            result.add(l);
                        }
                    }
                } else if (d == amount) {
                    List<Integer> l = new ArrayList<>();
                    l.add(d);
                    result.add(l);
                }
            }

            return result;
        } else {
            return Collections.emptyList();
        }
    }

    public static void prinSolution(List<List<Integer>> solution) {
        for (List<Integer> l : solution) {
            for (Integer i : l) {
                System.out.print(i + " ");
            }

            System.out.println();
        }
    }
}
