package spacetyckiting.util;

import java.util.ArrayList;
import java.util.List;
import spacetyckiting.message.entity.Position;

public class Positions {

    /**
     * Calculate distance between two positions
     * 
     * @param a First position
     * @param b Second Position
     * @return Distance between first and second position
     */
    public static int distance(Position a, Position b) {

        int dx = Math.abs(a.x - b.x);
        int dy = Math.abs(a.y - b.y);
        int dz = Math.abs(a.x + a.y - b.x - b.y);

        return Math.max(dx, Math.max(dy, dz));
    }

    /**
     * Calculate positions within a specified distance from a position
     * 
     * @param pos Starting position
     * @param radius radius from position
     * @return Array of positions within <code>radius</code> of <code>pos</code>
     */
    public static Position[] neighbours(Position pos, int radius) {
        List<Position> result = new ArrayList<>();
        for (int x = pos.x - radius; x <= pos.x + radius; x++) {
            for (int y = pos.y - radius; y <= pos.y + radius; y++) {
                Position newPos = new Position(x, y);
                if (distance(pos, newPos) <= radius && !pos.equals(newPos)) {
                    result.add(newPos);
                }
            }
        }
        return result.toArray(new Position[0]);
    }
    
    /**
     * Check if position is within field radius
     * 
     * @param pos Position to be checked
     * @param fieldRadius Radius of the field
     * @return true, if position is within field radius, otherwise false
     */
    public static boolean isWithinFieldRadius(Position pos, int fieldRadius) {
        // check that -14 <= x, y <= 14
        if(Math.abs(pos.x) <= fieldRadius && Math.abs(pos.y) <= fieldRadius) {
            // check that -14 <= x + y <= 14
            if(Math.abs(pos.x + pos.y) <= 14) {
                return true;
            }
        }
        return false;
    }
}
