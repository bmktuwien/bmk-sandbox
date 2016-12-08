import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by bmk on 12/7/16.
 */
public class Problem5 {

    public static void main(String[] args) {
        List<List<Integer>> sol1 = solve(21, new int[]{1, 2, 3, 4});
        int counter1 = solve2(21, new int[]{1, 2, 3, 4});
        prinSolution(sol1);
        System.out.println("Number of solutions: " + counter1);
    }

    public static List<List<Integer>> solve(int amount, int[] coins) {
        if (amount >= 0) {
            List<List<Integer>> result = new ArrayList<>();

            for (int d : coins) {
                if (amount > d) {
                    List<List<Integer>> subResult = solve(amount - d, coins);

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

    public static int solve2(int amount, int[] coins) {
        List<Integer> restAmounts = new ArrayList<>();
        restAmounts.add(amount);

        for (int coin : coins) {
            List<Integer> newRestAmounts = new ArrayList<>();

            for (int restAmount : restAmounts) {
                int counter = restAmount / coin;

                for (int i = 0; i <= counter; i++) {
                    newRestAmounts.add(restAmount - (i * coin));
                }
            }

            restAmounts = newRestAmounts;
        }

        int sol = 0;
        for (int restAmount : restAmounts) {
            if (restAmount == 0) {
                sol++;
            }
        }

        return sol;
    }

    public static void prinSolution(List<List<Integer>> solution) {
        for (List<Integer> l : solution) {
            for (Integer i : l) {
                System.out.print(i + " ");
            }

            System.out.println();
        }
        System.out.println("Number of solutions: " + solution.size());
    }
}
