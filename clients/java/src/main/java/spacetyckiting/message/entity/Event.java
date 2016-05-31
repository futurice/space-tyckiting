package spacetyckiting.message.entity;

import com.fasterxml.jackson.annotation.JsonValue;

public class Event {

    public enum EventType {
        HIT("hit"),
        DIE("die"),
        RADARECHO("radarEcho"),
        SEE("see"),
        SEEASTEROID("seeAsteroid"),
        DETECTED("detected"),
        DAMAGED("damaged"),
        MOVE("move"),
        NOACTION("noaction"),
        END("end");
        
        private String s;
        
        private EventType(String s) {
            this.s = s;
        }

        @JsonValue
        public String getString() {
            return s;
        }
    }
    
    /**
     * <p>Event types sent by the server</p>
     * <ul>
     * <li>HIT</li>
     * <li>DIE</li>
     * <li>RADARECHO</li>
     * <li>SEE</li>
     * <li>SEEASTEROID</li>
     * <li>DETECTED</li>
     * <li>DAMAGED</li>
     * <li>MOVE</li>
     * <li>NOACTION</li>
     * <li>END</li>
     * </ul>
     */
    public EventType event;
    
    /**
     * <ul>
     * <li>HIT: The id of bot that was hit</li>
     * <li>DIE: The id of the bot that dies
     * <li>SEE: The id of the bot that was seen</li>
     * <li>DETECTED: The id of the bot that was detected</li>
     * <li>DAMAGED: The id of the bot that was damaged</li>
     * <li>MOVE: The id of the bot that moved</li>
     * <li>NOACTION: The id of the bot that missed turn</li>
     * </ul>
     */
    public Integer botId;
    /**
     * <ul>
     * <li>HIT: the id of bot that shot the hit</li>
     * <li>SEE: the id of the bot that saw the target</li>
     * </ul>
     */
    public Integer source;
    /**
     * <ul>
     * <li>SEE: coordinates of the target</li>
     * <li>SEEASTEROID: coordinates of the asteroid</li>
     * <li>RADARECHO: coordinates of the detected target</li>
     * <li>MOVE: coordinates where you want to move</li>
     * </ul>
     */
    public Position pos;
    /** 
     * <ul>
     * <li>DAMAGED: amount of damage your bot took</li>
     * </ul>
     */
    public Integer damage;
        
    public static Event NoActionEvent(int botId) {
        Event e = new Event();
        e.event = EventType.NOACTION;
        e.botId = botId;
        return e;
    }
    
    public static Event MoveEvent(int botId, Position pos) {
        Event e = new Event();
        e.event = EventType.MOVE;
        e.botId = botId;
        e.pos = pos;
        return e;
    }
    
    public static Event DamagedEvent(int botId, int damage) {
        Event e = new Event();
        e.event = EventType.DAMAGED;
        e.botId = botId;
        e.damage = damage;
        return e;
    }
    
    public static Event DetectedEvent(int botId) {
        Event e = new Event();
        e.event = EventType.DETECTED;
        e.botId = botId;
        return e;
    }
    
    public static Event RadarEchoEvent(Position pos) {
        Event e = new Event();
        e.event = EventType.RADARECHO;
        e.pos = pos;
        return e;
    }
    
    public static Event SeeAsteroidEchoEvent(Position pos) {
        Event e = new Event();
        e.event = EventType.SEEASTEROID;
        e.pos = pos;
        return e;
    }
    
    public static Event SeeEvent(int source, int botId, Position pos) {
        Event e = new Event();
        e.event = EventType.SEE;
        e.source = source;
        e.botId = botId;
        e.pos = pos;
        return e;
    }
    
    public static Event HitEvent(int source, int botId) {
        Event e = new Event();
        e.event = EventType.HIT;
        e.source = source;
        e.botId = botId;
        return e;
    }
    
    public static Event DieEvent(int botId) {
        Event e = new Event();
        e.event = EventType.DIE;
        e.botId = botId;
        return e;
    }
}
