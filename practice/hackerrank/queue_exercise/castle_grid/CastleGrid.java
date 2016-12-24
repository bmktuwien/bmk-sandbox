import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * Created by bmk on 12/24/16.
 */
public class CastleGrid {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();

        boolean[][] blocked = new boolean[n][n];

        for (int i = 0; i < n; i++) {
            String l = scanner.next();

            for (int j = 0; j < n; j++) {
                if (l.charAt(j) == 'X') {
                    blocked[i][j] = true;
                }
            }
        }

        int a = scanner.nextInt();
        int b = scanner.nextInt();
        int c = scanner.nextInt();
        int d = scanner.nextInt();

        solve(blocked, n, new Point(a, b), new Point(c, d));
    }

    public static void solve(boolean[][] blocked, int n, Point origin, Point dest) {
        //TODO
    }

    public static List<Point> nextMoves(Point p, int n, boolean[][] blocked, boolean[][] visited) {
        List<Point> result = new ArrayList<>();

        for (int i = p.x + 1; i < n; i++) {
            if (blocked[i][p.y]) {
                break;
            }

            if (!visited[i][p.y]) {
                Point p2 = new Point(i, p.y);
                visited[i][p.y] = true;
                result.add(p2);
            }
        }

        for (int i = p.x - 1; i >= 0; i--) {
            if (blocked[i][p.y]) {
                break;
            }

            if (!visited[i][p.y]) {
                Point p2 = new Point(i, p.y);
                visited[i][p.y] = true;
                result.add(p2);
            }
        }

        for (int i = p.y + 1; i < n; i++) {
            if (blocked[p.x][i]) {
                break;
            }

            if (!visited[p.x][i]) {
                Point p2 = new Point(p.x, i);
                visited[p.x][i] = true;
                result.add(p2);
            }
        }

        for (int i = p.y - 1; i >= 0; i--) {
            if (blocked[p.x][i]) {
                break;
            }

            if (!visited[p.x][i]) {
                Point p2 = new Point(p.x, i);
                visited[p.x][i] = true;
                result.add(p2);
            }
        }

        return result;
    }

    public static class Point {
        int x;
        int y;
        int move;

        public Point(int x, int y) {
            this.x = x;
            this.y = y;
            this.move = 0;
        }

        @Override
        public boolean equals(Object other) {
            if (other instanceof Point) {
                Point p = (Point) other;
                return this.x == p.x && this.y == p.y;
            } else {
                return false;
            }
        }
    }
}
