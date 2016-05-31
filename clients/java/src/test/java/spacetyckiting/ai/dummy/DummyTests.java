package spacetyckiting.ai.dummy;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;
import spacetyckiting.ai.AI;
import spacetyckiting.message.entity.Action;
import spacetyckiting.message.entity.Bot;
import spacetyckiting.message.entity.Config;
import spacetyckiting.message.entity.Event;
import spacetyckiting.message.entity.Position;
import spacetyckiting.util.Positions;

public class DummyTests {

    private AI ai;
    
    private Config config;
    private int roundId;
    private List<Event> events;
    private List<Bot> bots;
    
    private int move;
    private int radar;
    private int cannon;

    @Before
    public void setUp() throws ClassNotFoundException, InstantiationException, IllegalAccessException {
        Class aiClass = Class.forName("spacetyckiting.ai.dummy.AIImpl");
        this.ai = (AI) aiClass.newInstance();

        config = new Config();
        config.asteroids = 0;
        config.bots = 4;
        config.cannon = 1;
        config.fieldRadius = 14;
        config.loopTime = 300;
        config.maxCount = 200;
        config.move = 2;
        config.noWait = false;
        config.radar = 3;
        config.see = 2;
        config.startHp = 10;

        roundId = 0;

        events = new ArrayList<>();

        bots = new ArrayList<>();
        bots.add(new Bot(0, "Bot 0", 0, 10, true, new Position(0, -14)));
        bots.add(new Bot(1, "Bot 1", 0, 10, true, new Position(14, -14)));
        bots.add(new Bot(2, "Bot 2", 0, 10, true, new Position(0, 14)));
        bots.add(new Bot(3, "Bot 3", 0, 10, true, new Position(-14, 14)));
        
        move = 0;
        radar = 0;
        cannon = 0;
    }

    @Test
    public void moveTest() {
        Set<Action> actions = ai.makeDecisions(roundId, events, bots, config);
        assertActions(actions);
        doActions(actions);
        assertEquals(4, move);

        nextRound();

        events.add(Event.DetectedEvent(bots.get(0).botId));
        events.add(Event.RadarEchoEvent(new Position(0, 0)));
        actions = ai.makeDecisions(roundId, events, bots, config);
        assertActions(actions);
        doActions(actions);
        assertEquals(8, move);
    }

    private void assertActions(Set<Action> actions) {
        for(Action a : actions) {
            if(a.type == Action.ActionType.MOVE) {
                for(Bot b : bots) {
                    if(Objects.equals(b.botId, a.botId)) {
                        assertTrue(Positions.distance(b.pos, a.pos) <= config.move);
                        assertTrue(!Objects.equals(b.pos, a.pos));
                    }
                }
            }
        }
    }

    private void doActions(Collection<Action> actions) {
        System.out.println("Round " + roundId);
        for(Action a : actions) {
            if (null != a.type) switch (a.type) {
                case MOVE:
                    move++;
                    moveBot(a.botId, a.pos);
                    break;
                case RADAR:
                    radar++;
                    break;
                case CANNON:
                    cannon++;
                    break;
            }
            System.out.println(a);
        }
    }
    
    private void nextRound() {
        roundId++;
        events.clear();
    }
    
    private void moveBot(int botId, Position pos) {
        for (Bot bot : bots) {
            if (Objects.equals(bot.botId, botId)) {
                // check that move action is within boundaries
                if(Positions.isWithinFieldRadius(pos, config.fieldRadius) && Positions.distance(bot.pos, pos) <= config.move) {
                    bot.pos = pos;
                }
            }
        }
    }
}
