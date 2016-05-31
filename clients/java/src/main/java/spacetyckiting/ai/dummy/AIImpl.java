package spacetyckiting.ai.dummy;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spacetyckiting.ai.AI;
import spacetyckiting.message.entity.Action;
import spacetyckiting.message.entity.Bot;
import spacetyckiting.message.entity.Config;
import spacetyckiting.message.entity.Event;
import spacetyckiting.message.entity.Position;
import spacetyckiting.util.Positions;

public class AIImpl implements AI {

    private static final Logger logger = LoggerFactory.getLogger(AIImpl.class);   
    private Random random = new Random();
    
    @Override
    public Set<Action> makeDecisions(int roundId, List<Event> events, List<Bot> bots, Config config) {
        Set<Action> actions = new LinkedHashSet<>(bots.size());
        for(Bot bot : bots) {
            Position[] ps = Positions.neighbours(bot.pos, config.move);
            Position pos = ps[random.nextInt(ps.length)];
            logger.debug("Bot {} moving from {} to {}!", bot.botId, bot.pos, pos);
            actions.add(new Action(Action.ActionType.MOVE, bot.botId, pos));
        }
        return actions;
    }
}
