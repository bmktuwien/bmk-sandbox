import java.util.*;

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

        solve(blocked, n, new Point(a, b, 0), new Point(c, d, 0));
    }

    public static void solve(boolean[][] blocked, int n, Point origin, Point dest) {
        Queue<Point> moves = new LinkedList<>();
        boolean[][] visited = new boolean[n][n];

        moves.add(origin);

        while (!moves.isEmpty()) {
            List<Point> next = nextMoves(moves.remove(), n, blocked, visited);

            for (Point p : next) {
                if (p.equals(dest)) {
                    System.out.println(p.move);
                    return;
                }

                moves.add(p);
            }
        }
    }

    public static List<Point> nextMoves(Point p, int n, boolean[][] blocked, boolean[][] visited) {
        List<Point> result = new ArrayList<>();

        for (int i = p.x + 1; i < n; i++) {
            if (blocked[i][p.y]) {
                break;
            }

            if (!visited[i][p.y]) {
                Point p2 = new Point(i, p.y, p.move + 1);
                visited[i][p.y] = true;
                result.add(p2);
            }
        }

        for (int i = p.x - 1; i >= 0; i--) {
            if (blocked[i][p.y]) {
                break;
            }

            if (!visited[i][p.y]) {
                Point p2 = new Point(i, p.y, p.move + 1);
                visited[i][p.y] = true;
                result.add(p2);
            }
        }

        for (int i = p.y + 1; i < n; i++) {
            if (blocked[p.x][i]) {
                break;
            }

            if (!visited[p.x][i]) {
                Point p2 = new Point(p.x, i, p.move + 1);
                visited[p.x][i] = true;
                result.add(p2);
            }
        }

        for (int i = p.y - 1; i >= 0; i--) {
            if (blocked[p.x][i]) {
                break;
            }

            if (!visited[p.x][i]) {
                Point p2 = new Point(p.x, i, p.move + 1);
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

        public Point(int x, int y, int move) {
            this.x = x;
            this.y = y;
            this.move = move;
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
