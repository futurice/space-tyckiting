#!/usr/bin/env python
import argparse
import logging
import websocket
import json
import importlib

from tyckiting_client import messages


def main():
    """
    Main method for running the client. Reads configuration from command line,
    and starts the client.
    """
    parser = argparse.ArgumentParser(description="Destroy 'em all")
    parser.add_argument('-H', '--host', default='localhost',
                        help="Host to connect to")
    parser.add_argument('-P', '--port', default=3000, type=int,
                        help="Port to connect to")
    parser.add_argument('-v', '--verbose', action='store_true',
                        help="Verbose output")
    parser.add_argument('-n', '--name', default='bot', help="Bot's name")
    parser.add_argument('-a', '--ai', default='dummy', help="Ai package")
    args = parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.DEBUG)
    else:
        logging.basicConfig(level=logging.INFO)

    client = TykitingClient(args.host, args.port, args.name, args.ai)
    client.run()


class TykitingClient():
    def __init__(self, host='localhost', port=3000, name='bot', ai='dummy'):
        """
        Initializes the client, and AI.

        Args:
            host: Host to connect to
            port: Port to connect to
            name: Team name to send to server on join
            ai: Ai to use, must be a module under the ai package
        """
        self.host = host
        self.port = port
        self.name = name

        package_name = 'tyckiting_client.ai.%s' % ai
        self.ai_package = importlib.import_module(package_name)
        self.ai = None

        self.team_id = None

    def on_connected(self, message):
        """
        Handles connected event from server. Responds to server with a
        join message.

        Args:
            message: Message received from server, containing the
                     configuration and team id.
        """
        logging.info('Connected to server with id %d', message.team_id)
        self.team_id = message.team_id
        self.ai = self.ai_package.Ai(message.team_id, message.config)
        self.send({
            'type': 'join',
            'teamName': self.name
        })

    def on_start(self, message):
        """
        Handles game start event from server.

        Args:
            message: Message from server containing team compositions
        """
        logging.info('Game started')

    def on_events(self, message):
        logging.info('Round %s', message.round_id)
        responses = self.ai.move(message.you.bots, message.events)
        actions = {
            'type': 'actions',
            'roundId': message.round_id,
            'actions': [response.to_dict() for response in responses]
        }
        self.send(actions)

    def on_end(self, message):
        """
        Handles game end event from server.

        Args:
            message: Message from server containing the winning team id
        """
        if message.winner_team_id == -1:
            result = "It's a draw."
        elif message.winner_team_id == self.team_id:
            result = "You win!"
        else:
            result = "You lose."

        logging.info('Game ended. %s', result)
        self.ws.close()

    def on_error(self, message):
        """
        Handles error event from server.

        Args:
            message: Message from server containing the error message.
        """
        logging.error(message.data)
        self.ws_close()

    def on_ws_open(self, ws):
        """
        Handles WebSocket open event

        Args:
            ws: WebSocket client instance
        """
        logging.debug('WebSocket connection opened')

    def on_ws_message(self, ws, raw_message):
        """
        Handles incoming messages from the WebSocket connection.

        Parses the message, and calls the corresponding `on_*` event handlers.

        Args:
            ws: WebSocket client instance
            raw_message: Message as s string
        """
        logging.debug('WebSocket message received %s', raw_message)
        dict_message = json.loads(raw_message)
        message_type = dict_message.get('type')

        if message_type == 'connected':
            self.on_connected(messages.Connected(**dict_message))
        elif message_type == 'start':
            self.on_start(messages.Start(**dict_message))
        elif message_type == 'events':
            self.on_events(messages.Events(**dict_message))
        elif message_type == 'end':
            self.on_end(messages.End(**dict_message))
        elif message_type == 'error':
            self.on_error(messages.Error(**dict_message))
        else:
            logging.warning('Unkown message type %s', message_type)

    def on_ws_error(self, ws, error):
        """
        Handles WebSocket error event.

        Args:
            ws: WebSocket client instance
            error: Error description string
        """
        logging.error('WebSocket connection error %s' % error)

    def on_ws_close(self, ws):
        """
        Handles WebSocket close event.

        Args:
            ws: WebSocket client instance

        """
        logging.debug('WebSocket connection closed')

    def send(self, message):
        """
        Serializes an object and sends it over the WebSocket connection.

        Args:
            message: JSON serializable object (dict or such)
        """
        message_string = json.dumps(message)
        logging.debug('Sending WebSocket message %s', message_string)
        self.ws.send(message_string)

    def run(self):
        """
        Connects to the WebSocket server, and listens for events.
        """
        url = "ws://%s:%s/" % (self.host, self.port)
        logging.info('Connecting to %s' % url)
        self.ws = websocket.WebSocketApp(url,
                                         on_open=self.on_ws_open,
                                         on_message=self.on_ws_message,
                                         on_error=self.on_ws_error,
                                         on_close=self.on_ws_close)

        self.ws.run_forever()


if __name__ == '__main__':
    """
    Call `main()` if the file is run from the commandline.
    """
    main()
