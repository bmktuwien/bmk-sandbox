import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

/**
 * Created by bmk on 12/7/16.
 */
public class Problem5 {

    public static void main(String[] args) {
        //List<List<Integer>> sol1 = solve(120, new int[]{1, 2, 3, 4});
        int counter1 = solve2(1000000, new int[]{100, 200, 300, 400, 500});
        int counter2 = solve3(1000000, new int[]{100, 200, 300, 400, 500});
        //printSolution(sol1);
        System.out.println("Number of solutions: " + counter1);
        System.out.println("Number of solutions: " + counter2);
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
        if (amount == 0) {
            return 0;
        }

        HashMap<Integer, Integer> restAmounts = new HashMap<>();
        restAmounts.put(amount, 1);
        int itCounter = 0;

        for (int coin : coins) {
            HashMap<Integer, Integer> newRestAmounts = new HashMap<>();

            for (int restAmount : restAmounts.keySet()) {
                int counter = restAmounts.get(restAmount);

                int k = restAmount / coin;
                for (int i = 0; i <= k; i++) {
                    int a = restAmount - i * coin;
                    int c = newRestAmounts.getOrDefault(a, 0);
                    newRestAmounts.put(a, c + counter);
                    itCounter++;
                }
            }

            restAmounts = newRestAmounts;
        }

        System.out.println("Solve2 itCounter: " + itCounter);
        return restAmounts.get(0);
    }

    // dynamic programming
    public static int solve3(int amount, int[] coins) {
        int[] possibilities = new int[amount+1];
        possibilities[0] = 1;
        int itCounter = 0;

        for (int coin : coins) {
            for (int a = coin; a <= amount; a++) {
                possibilities[a] += possibilities[a - coin];
                itCounter++;
            }
        }

        System.out.println("Solve3 itCounter: " + itCounter);
        return possibilities[amount];
    }

    public static void printSolution(List<List<Integer>> solution) {
        for (List<Integer> l : solution) {
            for (Integer i : l) {
                System.out.print(i + " ");
            }

            System.out.println();
        }
        System.out.println("Number of solutions: " + solution.size());
    }
}
