package spacetyckiting;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CLI {

    private static final Logger logger = LoggerFactory.getLogger(CLI.class);

    protected static final String USAGE = "-a, --ai <AI>          Select AI\n"
            + "-H, --host [host]      Host to connect to\n"
            + "-P, --port [port]      Port to connect to\n"
            + "-n, --teamname <name>  Bot's name\n";

    private String host = "localhost";
    private int port = 3000;
    private String teamName = "Anon.java";
    private String ai = "dummy";

    protected CLI(String[] args) {
        for (int i = 0; i < args.length; i++) {
            try {
                if ("-a".equals(args[i]) || "--ai".equals(args[i])) {
                    ai = args[++i];
                }
                else if ("-H".equals(args[i]) || "--host".equals(args[i])) {
                    host = args[++i];
                }
                else if ("-P".equals(args[i]) || "--port".equals(args[i])) {
                    port = Integer.parseInt(args[++i]);
                }
                else if ("-n".equals(args[i]) || "--teamname".equals(args[i])) {
                    teamName = args[++i];
                }
                else if ("-h".equals(args[i]) || "--help".equals(args[i]) || "/?".equals(args[i])) {
                    printUsage();
                    System.exit(0);
                }
                else {
                    logger.error("Unknown parameter: {}", args[i]);
                    printUsage();
                    System.exit(-1);
                }
            }
            catch (NumberFormatException nfe) {
                logger.error("Invalid port number: {}", args[i]);
                printUsage();
                System.exit(-1);
            }
            catch (Exception e) {
                e.printStackTrace();
                printUsage();
                System.exit(-1);
            }
        }
    }

    protected void start() {
        logger.info("Host: {}:{}", host, port);
        logger.info("Team: {}", teamName);
        logger.info("AI:   {}", ai);
        Client client = new Client(host, port, teamName, ai);
        client.start();
    }

    protected static void printUsage() {
        System.out.println(USAGE);
    }

    public static void main(String[] args) {
        CLI cli = new CLI(args);
        cli.start();
    }
}
