package spacetyckiting.message.entity;

public class Config {
    
    /** Number of bots */
    public Integer bots;
    /** Size of the hexgrid */
    public Integer fieldRadius;
    /** The maximum movement amount */
    public Integer move;
    /** Starting hit points */
    public Integer startHp;
    /** Cannon radius, direct damage is radius + 1, damage decay linearly as function of distance */
    public Integer cannon;
    /** Radar radius, inclusive */
    public Integer radar;
    /** Sight radius, inclusive */
    public Integer see;
    /** Game length in rounds */
    public Integer maxCount;
    /** Number of asteroids */
    public Integer asteroids;
    /** Delay between turns */
    public Integer loopTime;
    /** Start next round if all players have registered their actions */
    public boolean noWait;

    @Override
    public String toString() {
        return "Config{" + "bots=" + bots + ", fieldRadius=" + fieldRadius + ", move=" + move + ", startHp=" + startHp + ", cannon=" + cannon + ", radar=" + radar + ", see=" + see + ", maxCount=" + maxCount + ", asteroids=" + asteroids + ", loopTime=" + loopTime + ", noWait=" + noWait + '}';
    }
}
