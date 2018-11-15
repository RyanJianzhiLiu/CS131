import asyncio
import aiohttp
import sys
import json
import time
import logging
from format_checkers import valid_coord, valid_time, valid_radius, valid_max, valid_PEER

# server-port info
host = '127.0.0.1'
srv2port = {'Goloman':11213,
            'Hands':11214,
            'Holiday':11215,
            'Welsh':11216,
            'Wilkes':11217}

srv2peers = {'Goloman':['Hands','Holiday','Wilkes'],
             'Hands'  :['Goloman','Wilkes'],
             'Holiday':['Goloman','Welsh','Wilkes'],
             'Welsh'  :['Holiday'],
             'Wilkes' :['Goloman','Hands','Holiday']}

# toy herd model
# srv2peers = {'Goloman':['Hands'], 'Hands':['Goloman']}

myDatabase = {}
myAPIkey = 'AIzaSyA7lJIidoEdefGNThTmtmGj0Z7d7FJ6lEU'

async def fetch(session, url, lat_lng, r):
    params = {'key':myAPIkey, 'location':lat_lng, 'radius':r}
    async with session.get(url, params=params) as response:
        logging.debug('Request Addr:\n\t{}'.format(str(response.url)))
        return await response.json()

async def search_resp(lat_lng, r, max_info, transport):
    url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?'
    async with aiohttp.ClientSession() as session:
        places = await fetch(session, url, lat_lng, r)
    if places['status'] != 'OK':
        if places['status'] != 'ZERO_RESULTS':
            logging.warning('Request to Google Place went wrong.')
    else:
        all_results = places['results']
        if len(all_results) >= max_info:
            places['results'] = all_results[0:max_info]
        logging.debug('{0}/{1} places sent to client.'.format(len(places['results']),len(all_results)))
    response = json.dumps(places,indent=2)
    transport.write(response.encode())
    transport.write('\n\n'.encode())
    logging.info('Send:\n{}\n'.format(response))

class MyProtocol(asyncio.Protocol):
    def __init__(self, loop, srv):
        self.loop = loop
        self.srv = srv
        self.clt = None
        
    def connection_made(self, transport):
        self.clt = transport.get_extra_info('peername')
        logging.info('Connection from {}'.format(self.clt))
        self.transport = transport

    def connection_lost(self, exc):
        logging.info('Disconnected from {}'.format(self.clt))
        
    def data_received(self, data_bytes):
        data = data_bytes.decode()
        logging.info('Data received:\n\t{!r}'.format(data))
        self.respond(data)

    def respond(self, command):
        argv = command.split()
        # handle msg from peer server
        if (len(argv) == 6) and (valid_PEER(argv[0])):
            try:
                self.handle_PEER(argv[1:]); return
            except (ValueError, TypeError) as e:
                logging.critical('Malicious command:\n\t{!r}'.format(command))
                self.handle_invalid(command); return
        # handle command from client
        if len(argv) != 4:
            logging.error('Wrong number of arguments. {}/4'.format(len(argv)))
            self.handle_invalid(command); return
        cmd_name = argv[0]
        if cmd_name == 'IAMAT':
            self.handle_IAMAT(argv[1:],command)
        elif cmd_name == 'WHATSAT':
            self.handle_WHATSAT(argv[1:],command)
        else:
            logging.error('Invalid command name. Use IAMAT or WHATSAT.')
            self.handle_invalid(command)

    def handle_invalid(self, command):
        response = '? '+command
        self.transport.write(response.encode())
        logging.info('Send:\n\t{!r}'.format(response))

    def handle_IAMAT(self, info, command):
        clt_id, coord, time_str = info
        # check location format
        try:
            lat, lng = valid_coord(coord)
        except TypeError:
            logging.error('Invalid location format: {}'.format(coord))
            self.handle_invalid(command); return
        # check time format
        try:
            user_time = valid_time(time_str)[0]
        except TypeError:
            logging.error('Invalid time: {}'.format(time_str))
            self.handle_invalid(command); return
        lag = time.time() - user_time
        if lag > 0: lag_str = '+'+str(lag)
        else: lag_str = str(lag)
        # respond to client
        response = ('AT {0} {1} {2} {3} {4}\n'
                    .format(self.srv, lag_str, clt_id, coord, time_str))
        self.transport.write(response.encode())
        logging.info('Send:\n\t{!r}'.format(response))
        # update server's database (& prop)
        new_data = (self.srv, lag, coord, time_str)
        self.update_database(clt_id,new_data)

    def handle_WHATSAT(self, info, command):
        clt_id, r_str, max_info_str = info
        # check radius format
        try:
            radius = valid_radius(r_str)[0]
            radius *= 1000
        except TypeError:
            logging.error('Invalid radius: {}'.format(r_str))
            self.handle_invalid(command); return
        # check max_info format
        try:
            max_info = valid_max(max_info_str)[0]
        except TypeError:
            logging.error('Invalid bound for number of places: {}'\
                          .format(max_info_str))
            self.handle_invalid(command); return
        # response1
        if clt_id not in myDatabase.keys():
            logging.error('Client {} not found.'.format(clt_id))
            self.handle_invalid(command); return
        srv, lag, coord, time_str = myDatabase[clt_id]
        if lag > 0: lag_str = '+'+str(lag)
        else: lag_str = str(lag)
        response1 = ('AT {0} {1} {2} {3} {4}\n'
                     .format(srv, lag_str, clt_id, coord, time_str))
        self.transport.write(response1.encode())
        logging.info('Send:\n\t{!r}'.format(response1))
        # response2
        lat, lng = valid_coord(coord)
        lat_lng = '{0},{1}'.format(lat,lng)
        logging.debug('Request to GooglePlaces. loc:{0} rad:{1}m max_info:{2}'\
                      .format(lat_lng,radius,max_info))
        coro = search_resp(lat_lng, str(radius), max_info, self.transport)
        self.loop.create_task(coro)
        
    def handle_PEER(self, info):
        #info = [srv, lag, coord, time_str, clt_id], str
        clt_id = info[4]
        srv, lag_str, coord, time_str = info[0:4]
        # make sure everything is valid
        i = list(srv2port.keys()).index(srv)
        lag = float(lag_str)
        lat, lng = valid_coord(coord)
        user_time = valid_time(time_str)[0]
        new_data = (srv, lag, coord, time_str)
        self.update_database(clt_id, new_data)
        
    def update_database(self, clt_id, new_data):
        history = myDatabase.get(clt_id)
        if (history != new_data):
            if (history) and (float(history[3])>float(new_data[3])):
                logging.debug('Obsolete \'new\' data. {0}(old)>{1}(new)'\
                              .format(float(history[3]),float(new_data[3])))
                return
            myDatabase[clt_id] = new_data
            logging.debug('Database updated.')
            new_data_msg = ('PEER{5} {0} {1} {2} {3} {4}'
                            .format(new_data[0], new_data[1],
                                    new_data[2], new_data[3],
                                    clt_id, self.srv))
            logging.info('Propagate message:\n\t{}'.format(new_data_msg))
            self.loop.create_task(self.prop(new_data_msg.encode()))
        else:
            logging.debug('Already in database:\n\t{0}:{1}'.format(clt_id,history))
        
    async def prop(self, data_bytes):
        for peer_srv in srv2peers[self.srv]:
            port = srv2port[peer_srv]
            try:
                coro = asyncio.open_connection(host,port,loop=self.loop)
                reader, writer = await coro
                writer.write(data_bytes)
                await writer.drain(); writer.close()
                logging.info('Message sent to {0}'.format(peer_srv))
            except ConnectionRefusedError:
                logging.warning('Failed connecting to {0}.'.format(peer_srv))


def main():
    if len(sys.argv) != 2:
        print("Usage: python3 server.py server_name")
        exit()
    srv = sys.argv[1]
    try: port = srv2port[srv]
    except:
        print("Error: Server name does not exist.")
        exit()
    
    logging.basicConfig(level=logging.DEBUG,
                        format='%(asctime)s %(levelname)s %(message)s',
                        filename = '{}.log'.format(srv))

    loop = asyncio.get_event_loop()
    coro = loop.create_server(lambda:MyProtocol(loop,srv),host,port)
    server = loop.run_until_complete(coro)

    logging.info('Serving on {}'.format(server.sockets[0].getsockname()))
    try:
        loop.run_forever()
    except KeyboardInterrupt:
        pass

    logging.info('Closing server.\n\n')
    server.close()
    loop.run_until_complete(server.wait_closed())
    loop.close()

if __name__ == '__main__':
    main()
