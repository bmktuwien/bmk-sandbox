/**
 * Created by bmk on 12/8/16.
 */
public class Problem6 {

    public static void main(String[] args) {
        Rectangle r1 = new Rectangle(0, 0, 50, 50);
        Rectangle r2 = new Rectangle(5, 5, 10, 10);
        System.out.println(r1.intersect(r2));
    }

    public static class Rectangle {

        // coordinates of bottom left corner
        int leftX;
        int bottomY;

        // dimensions
        int width;
        int height;

        public Rectangle(int leftX, int bottomY, int width, int height) {
            this.leftX = leftX;
            this.bottomY = bottomY;
            this.width  = width;
            this.height = height;
        }

        private Rectangle() {

        }

        public String toString() {
            return String.format("(%d, %d, %d, %d)", leftX, bottomY, width, height);
        }

        public Rectangle intersect(Rectangle other) {
            Rectangle result = new Rectangle();
            boolean xIntersected = false;
            boolean yIntersected = false;

            // x-axis intersection
            if ((other.leftX > leftX && other.leftX < leftX + width) ||
                    (leftX > other.leftX && leftX < other.leftX + other.width)){
                result.leftX = Math.max(leftX, other.leftX);
                result.width = Math.min(leftX + width, other.leftX + other.width) - result.leftX;
                xIntersected = true;
            }

            // y-axis intersection
            if ((other.bottomY > bottomY && other.bottomY < bottomY + height) ||
                    (bottomY > other.bottomY && bottomY < other.bottomY + other.height)) {
                result.bottomY = Math.max(bottomY, other.bottomY);
                result.height = Math.min(bottomY + height, other.bottomY + other.height) - result.bottomY;
                yIntersected = true;
            }

            if (xIntersected && yIntersected) {
                return result;
            } else {
                return null;
            }
        }
    }
}
