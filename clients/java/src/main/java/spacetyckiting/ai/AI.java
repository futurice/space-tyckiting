package spacetyckiting.ai;

import java.util.List;
import java.util.Set;
import spacetyckiting.message.entity.Action;
import spacetyckiting.message.entity.Bot;
import spacetyckiting.message.entity.Config;
import spacetyckiting.message.entity.Event;

public interface AI {
    
    /**
     * Make decisions for next round of the game.
     * 
     * @param roundId Id of current round
     * @param events List containing incoming game events
     * @param bots List of bots that AI controls
     * @param config Configuration of the game
     * @return Set of actions that AI should perform this round.
     */
    public Set<Action> makeDecisions(int roundId, List<Event> events, List<Bot> bots, Config config);
    
}
