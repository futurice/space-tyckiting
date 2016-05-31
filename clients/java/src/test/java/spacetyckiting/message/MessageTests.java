package spacetyckiting.message;

import com.fasterxml.jackson.annotation.JsonInclude;
import spacetyckiting.message.entity.Config;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.File;
import java.io.IOException;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

public class MessageTests {

    private ObjectMapper mapper;

    @Before
    public void setUp() {
        mapper = new ObjectMapper();
        mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
    }

    @Test
    public void configJsonTest() throws IOException {

        // Convert JSON string to Object
        String jsonInString = "{\"bots\":3,\"fieldRadius\":14,\"move\":2,\"startHp\":10,\"cannon\":1,\"radar\":3,\"see\":2,\"maxCount\":200,\"asteroids\":0,\"loopTime\":300,\"noWait\":false}";
        Config config2 = mapper.readValue(jsonInString, Config.class);
        System.out.println(config2);

        //Pretty print
        String prettyConfig = mapper.writerWithDefaultPrettyPrinter().writeValueAsString(config2);
        System.out.println(prettyConfig);

        // Convert Object to JSON
        String jsonFromObj = mapper.writeValueAsString(config2);
        System.out.println(jsonFromObj);
        assertEquals(jsonInString, jsonFromObj);
    }
    
    @Test
    public void startMsgTest() throws JsonProcessingException {
        
        Message msg = new Message();
        msg.type = Message.MessageType.START;
        
        String json = mapper.writeValueAsString(msg);
        System.out.println(json);
        
        assertEquals("{\"type\":\"start\"}", json);
    }
}
