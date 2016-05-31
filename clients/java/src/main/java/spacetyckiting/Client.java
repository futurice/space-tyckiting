package spacetyckiting;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import javax.websocket.ClientEndpointConfig;
import javax.websocket.Endpoint;
import javax.websocket.EndpointConfig;
import javax.websocket.MessageHandler;
import javax.websocket.Session;
import org.apache.commons.lang3.time.StopWatch;
import org.glassfish.tyrus.client.ClientManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spacetyckiting.ai.AI;
import spacetyckiting.message.Message;
import spacetyckiting.message.entity.Action;

public class Client {

    private static final Logger logger = LoggerFactory.getLogger(Client.class);
    private static final Logger recvLog = LoggerFactory.getLogger("recvLog");
    private static final Logger sendLog = LoggerFactory.getLogger("sendLog");

    private String teamName;
    private Integer teamId;
    private String url;
    private AI ai;

    private final ObjectMapper mapper;
    private CountDownLatch messageLatch;
    private StopWatch stopwatch;

    Client(String host, int port, String teamName, String aiName) {
        this.teamName = teamName;
        this.url = "ws://" + host + ":" + port;

        // Instantiate AI
        try {
            Class aiClass = Class.forName("spacetyckiting.ai." + aiName + ".AIImpl");
            this.ai = (AI) aiClass.newInstance();
        }
        catch (Exception e) {
            logger.error("Unable to instantiate AI \"" + aiName + "\". Exiting.", e);
            System.exit(-1);
        }

        mapper = new ObjectMapper();
        // Prevents null values in serialization
        mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
    }

    protected void start() {

        try {
            messageLatch = new CountDownLatch(1);
            stopwatch = new StopWatch();

            final ClientEndpointConfig cec = ClientEndpointConfig.Builder.create().build();

            ClientManager client = ClientManager.createClient();
            client.connectToServer(new Endpoint() {

                @Override
                public void onOpen(Session session, EndpointConfig config) {
                    try {
                        session.addMessageHandler(new MessageHandler.Whole<String>() {

                            @Override
                            public void onMessage(String json) {
                                stopwatch.reset();
                                stopwatch.start();
                                recvLog.info(json);

                                try {
                                    Message incomingMsg = mapper.readValue(json, Message.class);
                                    switch(incomingMsg.type) {
                                        case CONNECTED:
                                            teamId = incomingMsg.teamId;
                                            break;
                                        case START:
                                            // do nothing
                                            break;
                                        case EVENTS:
                                            Set<Action> actions = ai.makeDecisions(incomingMsg.roundId, incomingMsg.events, incomingMsg.you.bots, incomingMsg.config);
                                            Message outgoingMsg = new Message();
                                            outgoingMsg.type = Message.MessageType.ACTIONS;
                                            outgoingMsg.roundId = incomingMsg.roundId;
                                            outgoingMsg.actions = new ArrayList<>(actions);
                                            String outgoingJson = mapper.writeValueAsString(outgoingMsg);
                                            session.getAsyncRemote().sendText(outgoingJson);
                                            sendLog.info(outgoingJson);
                                            break;
                                        case END:
                                            logger.info("Game ended. {}", teamId.equals(incomingMsg.winnerTeamId) ? "You win!" : incomingMsg.winnerTeamId < 0 ? "The game was a draw" : "You lose.");
                                            messageLatch.countDown();
                                            break;
                                    }
                                }
                                catch (IOException e) {
                                    logger.error("Unable to parse message: " + json, e);
                                }
                                catch (Exception e) {
                                    logger.error("Unable to take action!!", e);
                                }
                                stopwatch.stop();
                                logger.debug("Actions concluded in {} ms!", stopwatch.getTime());
                            }
                        });
                        Message joinMsg = new Message();
                        joinMsg.type = Message.MessageType.JOIN;
                        joinMsg.teamName = teamName;
                        session.getBasicRemote().sendText(mapper.writeValueAsString(joinMsg));
                    }
                    catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }, cec, new URI(url));
            messageLatch.await(300, TimeUnit.SECONDS);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }
}
