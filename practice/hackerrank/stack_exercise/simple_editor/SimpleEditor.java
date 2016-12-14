import java.util.Scanner;
import java.util.Stack;

/**
 * Created by bmk on 12/12/16.
 */
public class SimpleEditor {

    private static Scanner scanner = new Scanner(System.in);

    public static void main(String[] args) {
        int n = scanner.nextInt();
        StringBuilder result = new StringBuilder();
        Stack<Undo> undoHistory = new Stack<>();

        for (int i = 0; i < n; i++) {
            int cmd = scanner.nextInt();

            switch (cmd) {
                case 1:
                    String s = scanner.next();
                    result.append(s);
                    undoHistory.push(new AppendUndo(s.length()));
                    break;
                case 2:
                    int k = scanner.nextInt();
                    String deleted = result.substring(result.length() - k);
                    result.delete(result.length() - k, result.length());
                    undoHistory.push(new DeleteUndo(deleted));
                    break;
                case 3:
                    k = scanner.nextInt();
                    System.out.println(result.charAt(k - 1));
                    break;
                case 4:
                    Undo u = undoHistory.pop();

                    if (u instanceof AppendUndo) {
                        AppendUndo au = (AppendUndo) u;
                        result = result.delete(result.length() - au.k, result.length());
                    } else if (u instanceof  DeleteUndo) {
                        DeleteUndo du = (DeleteUndo) u;
                        result.append(du.s);
                    }
            }
        }
    }

    abstract static class Undo {

    }

    static class AppendUndo extends Undo {
        final int k;

        AppendUndo (int k) {
            this.k = k;
        }
    }

    static class DeleteUndo extends Undo {
        final String s;

        DeleteUndo (String s) {
            this.s = s;
        }
    }
}
