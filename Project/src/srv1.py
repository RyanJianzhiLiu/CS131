import asyncio
import sys
import time
from Gplaces import search_resp
from format_checkers import valid_coord, valid_time, valid_radius, valid_max

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
"""
srv2peers = {'Goloman':['Hands'],
             'Hands'  :['Goloman']}
"""
myDatabase = {}

class MyProtocol(asyncio.Protocol):
    def __init__(self, loop, srv):
        self.loop = loop
        self.srv = srv

    def connection_made(self, transport):
        self.src_port = transport.get_extra_info('peername')[1]
        print('Connection from {}'.format(self.src_port))
        self.transport = transport

    def data_received(self, data_bytes):
        data = data_bytes.decode()
        print('Data received: {!r}'.format(data))
        self.respond(data)
        #print('Send: {!r}'.format(message))
        #self.transport.write(data)

        #print('Close the client socket')
        #self.transport.close()

    def respond(self, command):
        argv = command.split()
        # handle msg from peer server
        if (len(argv) == 6) and (argv[0] == 'PEER4027'):
            self.handle_PEER(argv[1:]); return
        # handle command from client
        if len(argv) != 4:
            print('Wrong number of arguments. {}/4'.format(len(argv)))
            self.handle_invalid(command); return
        cmd_name = argv[0]
        if cmd_name == 'IAMAT':
            self.handle_IAMAT(argv[1:],command)
        elif cmd_name == 'WHATSAT':
            self.handle_WHATSAT(argv[1:],command)
        else:
            print('Invalid command name. (IAMAT|WHATSAT)')
            self.handle_invalid(command)
            
    def handle_invalid(self, command):
        response = '? '+command
        self.transport.write(response.encode())

    def handle_IAMAT(self, info, command):
        clt_id, coord, time_str = info
        # check location format
        try:
            lat, lng = valid_coord(coord)
        except TypeError:
            print('Invalid location format: {}'.format(coord))
            self.handle_invalid(command); return
        # check time format
        try:
            user_time = valid_time(time_str)[0]
        except TypeError:
            print('Invalid time: {}'.format(time_str))
            self.handle_invalid(command); return
        # update server's database
        lag = time.time() - user_time
        new_data = (self.srv, lag, coord, time_str)
        self.update_database(clt_id,new_data)
        # respond to client
        response = ('AT {0} {1} {2} {3} {4}\r\n'
                    .format(self.srv, lag, clt_id, coord, time_str))
        self.transport.write(response.encode())

    def handle_WHATSAT(self, info, command):
        clt_id, r_str, max_info_str = info
        # check radius format
        try:
            radius = valid_radius(r_str)[0]
        except TypeError:
            print('Invalid radius: {}'.format(r_str))
            self.handle_invalid(command); return
        # check max_info format
        try:
            max_info = valid_max(max_info_str)[0]
        except TypeError:
            print('Invalid bound for number of places: {}'\
                  .format(max_info_str))
            self.handle_invalid(command); return
        # response1
        if clt_id not in myDatabase.keys():
            print('Client {} not found.'.format(clt_id))
            self.handle_invalid(command); return
        srv, lag, coord, time_str = myDatabase[clt_id]
        response1 = ('AT {0} {1} {2} {3} {4}\r\n'
                     .format(srv, lag, clt_id, coord, time_str))
        self.transport.write(response1.encode())
        # response2
        lat, lng = valid_coord(coord)
        lat_lng = '{0},{1}'.format(lat,lng)
        coro = search_resp(lat_lng, r_str, max_info, self.transport)
        self.loop.create_task(coro)
        print('Request info from Google Places. loc:{0} rad:{1}'.format(lat_lng,r_str))
        
    def handle_PEER(self, info):
        clt_id = info[4]
        srv, lag_str, coord, time_str = info[0:4]
        new_data = (srv, float(lag_str), coord, time_str)
        self.update_database(clt_id, new_data)
        
    def update_database(self, clt_id, new_data):
        history = myDatabase.get(clt_id)
        if (history != new_data):
            myDatabase[clt_id] = new_data
            new_data_msg = ('PEER4027 {0} {1} {2} {3} {4}'
                            .format(new_data[0],new_data[1],
                                    new_data[2],new_data[3],clt_id))
            print("Database updated. Propagate message: {}"\
                  .format(new_data_msg))
            self.loop.create_task(self.prop(new_data_msg.encode()))
        else:
            print("Already in database: {0}:{1}".format(clt_id,history))
        
    async def prop(self, data_bytes):
        for peer_srv in srv2peers[self.srv]:
            port = srv2port[peer_srv]
            try:
                coro = asyncio.open_connection(host,port,loop=self.loop)
                reader, writer = await coro
                writer.write(data_bytes)
                await writer.drain(); writer.close()
            except ConnectionRefusedError:
                print('Attempt to connect with {0} failed.'.format(peer_srv))
                
            
if len(sys.argv) != 2:
    print("Usage: python3 server.py server_name")
    exit()
srv = sys.argv[1]
try: port = srv2port[srv]
except:
    print("Err: Server name does not exist.")
    exit()

loop = asyncio.get_event_loop()
# Each client connection will create a new protocol instance
coro = loop.create_server(lambda:MyProtocol(loop,srv),host,port)
server = loop.run_until_complete(coro)

# Serve requests until Ctrl+C is pressed
print('Serving on {}'.format(server.sockets[0].getsockname()))
try:
    loop.run_forever()
except KeyboardInterrupt:
    pass

# Close the server
server.close()
loop.run_until_complete(server.wait_closed())
loop.close()
