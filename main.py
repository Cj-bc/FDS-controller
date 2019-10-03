import faceDataServer_pb2_grpc as grpc_faceDataServer
from faceDataServer_pb2 import FaceData, Token, Status
from concurrent import futures
import curses
import grpc
import time
import secrets
from typing import List

# FaceDataStore {{{
class FaceDataStore():
    """ Stores current FaceData
    """
    current: FaceData = FaceData(x=0.0, y=0.0, z=0.0)

    def addy(self, amount):
        self.current.y += amount

    def addx(self, amount):
        self.current.x += amount

    def addz(self, amount):
        self.current.z += amount
# }}}


class Servicer(grpc_faceDataServer.FaceDataServerServicer):
    dataStore: FaceDataStore = None
    clients: List[str] = []

    def __init__(self, ds):
        self.dataStore = ds

    def init(self, req, context):
        return Status(success=True, token=Token(token=secrets.token_hex()))

    def startStream(self, req, context):
        self.clients.append(req.token)

        while req.token in self.clients:
            c = self.dataStore.current
            yield FaceData(c.x, c.y, c.z)

    def stopStream(self, req, context):
        if req.token in self.clients:
            self.clients.remove(req.token)
        return Status(success=True)

    def shutdown(self, req, context):
        return Status(success=True)




def main(stdscr):
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    dataStore = FaceDataStore()
    grpc_faceDataServer.add_FaceDataServerServicer_to_server(
            Servicer(dataStore), server)
    server.add_insecure_port('[::]:5039')
    server.start()
    print("----- Server started -----")
    try:
        while True:
            k = stdscr.getkey()
            if k == 'Q':
                break
            elif k == 'a':
                dataStore.addy(-1)
            elif k == 'd':
                dataStore.addy(1)
            elif k == 'w':
                dataStore.addx(1)
            elif k == 's':
                dataStore.addx(-1)
            elif k == 'q':
                dataStore.addz(-1)
            elif k == 'e':
                dataStore.addz(1)

    except KeyboardInterrupt:
        pass
    finally:
        server.stop(0)
        print("Server stopped.")


curses.wrapper(main)
