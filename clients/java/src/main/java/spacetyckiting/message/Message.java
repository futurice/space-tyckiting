package spacetyckiting.message;

import com.fasterxml.jackson.annotation.JsonValue;
import spacetyckiting.message.entity.Event;
import java.util.List;
import spacetyckiting.message.entity.Action;
import spacetyckiting.message.entity.Config;
import spacetyckiting.message.entity.Team;

public class Message {

    public enum MessageType {
        CONNECTED("connected"),
        JOIN("join"),
        START("start"),
        EVENTS("events"),
        ACTIONS("actions"),
        END("end");
        
        private String s;
        
        private MessageType(String s) {
            this.s = s;
        }

        @JsonValue
        public String getString() {
            return s;
        }
    }

    public MessageType type;
    /** <ul>
     *  <li>CONNECTED: Id of the team</li>
     *  </ul>
     */
    public Integer teamId;
    public String teamName;
    public Integer winnerTeamId;
    public Integer roundId;
    public Config config;
    public Team you;
    public List<Action> actions;
    public List<Team> otherTeams;
    public List<Event> events;
}
