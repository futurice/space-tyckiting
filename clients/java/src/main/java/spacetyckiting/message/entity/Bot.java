package spacetyckiting.message.entity;

public class Bot {

    public Integer botId;
    public String name;
    public Integer teamId;
    public Integer hp;
    public Boolean alive;
    public Position pos;

    public Bot() {
        
    }
    
    public Bot(Integer botId, String name, Integer teamId, Integer hp, Boolean alive, Position pos) {
        this.botId = botId;
        this.name = name;
        this.teamId = teamId;
        this.hp = hp;
        this.alive = alive;
        this.pos = pos;
    }
}
