package spacetyckiting.message.entity;

import com.fasterxml.jackson.annotation.JsonValue;
import java.util.Objects;

public class Action {

    public enum ActionType {
        MOVE("move"),
        RADAR("radar"),
        CANNON("cannon");
        
        private String s;
        
        private ActionType(String s) {
            this.s = s;
        }

        @JsonValue
        public String getString() {
            return s;
        }
    }
    
    public Action() {}
    
    public Action(ActionType type, Integer botId, Position pos) {
        this.type = type;
        this.botId = botId;
        this.pos = pos;
    }
    
    /**
     * <p>Bot's action, which can be one of the following</p>
     * <ul>
     * <li>MOVE: Move to target hex position</li>
     * <li>RADAR: Radar scan target hex position</li>
     * <li>CANNON: Shoot tarhet hex position</li>
     * </ul>
     */
    public ActionType type;
    /** Id of the bot taking action */
    public Integer botId;
    /** Target hex coordinates */
    public Position pos;

    @Override
    public int hashCode() {
        int hash = 5;
        hash = 61 * hash + Objects.hashCode(this.type);
        hash = 61 * hash + Objects.hashCode(this.botId);
        hash = 61 * hash + Objects.hashCode(this.pos);
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final Action other = (Action) obj;
        if (this.type != other.type) {
            return false;
        }
        if (!Objects.equals(this.botId, other.botId)) {
            return false;
        }
        if (!Objects.equals(this.pos, other.pos)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "Action{" + "type=" + type + ", botId=" + botId + ", pos=" + pos + '}';
    }
}
