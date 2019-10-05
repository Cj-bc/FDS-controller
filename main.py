import faceDataServer_pb2_grpc as grpc_faceDataServer
from faceDataServer_pb2 import FaceData, Token, Status
from concurrent import futures
import curses
from curses.textpad import Textbox, rectangle
import grpc
import time
import secrets
import math
from typing import List

# FaceDataStore {{{
class FaceDataStore():
    """ Stores current FaceData
    """
    current: FaceData = FaceData(x=0.0, y=0.0, z=0.0)

    def addy(self, amount):
        self.current.y += math.radians(amount)

    def addx(self, amount):
        self.current.x += math.radians(amount)

    def addz(self, amount):
        self.current.z += math.radians(amount)
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
            yield self.dataStore.current

    def stopStream(self, req, context):
        if req.token in self.clients:
            self.clients.remove(req.token)
        return Status(success=True)

    def shutdown(self, req, context):
        return Status(success=True)


def askDirectDegree(stdscr) -> Tuple[str, Degree]:
    editwin = curses.newwin(1,15, 20, 1)
    rectangle(stdscr, 20-1, 1-1, 20+1, 15+1+1)
    stdscr.refresh()
    box = Textbox(editwin)

    box.edit()

    gathered = box.gather()
    try:
        res = int(gathered)
    except ValueError:
        stdscr.addstr(1, 1, "Failed to input")
        return 0

    return res



def main(stdscr):
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    dataStore = FaceDataStore()
    grpc_faceDataServer.add_FaceDataServerServicer_to_server(
            Servicer(dataStore), server)
    server.add_insecure_port('[::]:50052')
    server.start()
    print("----- Server started -----")
    try:
        while True:
            k = stdscr.getkey()
            if k == 'Q':
                break
            elif k == 'a':
                dataStore.addy(-1)
                stdscr.addch(20, 20, '<')
            elif k == 'd':
                dataStore.addy(1)
                stdscr.addch(20, 20, '>')
            elif k == 'w':
                dataStore.addx(1)
                stdscr.addch(20, 20, '^')
            elif k == 's':
                dataStore.addx(-1)
                stdscr.addch(20, 20, 'v')
            elif k == 'q':
                dataStore.addz(-1)
                stdscr.addstr(20, 20, '/>')
            elif k == 'e':
                dataStore.addz(1)
                stdscr.addstr(20, 20, '<\\')
            elif k == 'x':
                degree = askDirectDegree(stdscr)
                dataStore.current.x = math.radians(degree)
            elif k == 'y':
                degree = askDirectDegree(stdscr)
                dataStore.current.y = math.radians(degree)
            elif k == 'z':
                degree = askDirectDegree(stdscr)
                dataStore.current.z = math.radians(degree)

            stdscr.addstr(10, 10, f"x: {math.degrees(dataStore.current.x)}, "
                                  f"y: {math.degrees(dataStore.current.y)}, "
                                  f"z: {math.degrees(dataStore.current.z)}")

    except KeyboardInterrupt:
        pass
    finally:
        server.stop(0)
        print("Server stopped.")


curses.wrapper(main)
