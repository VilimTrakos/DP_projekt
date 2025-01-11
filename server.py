import socket
import threading
import tkinter as tk
from tkinter import Canvas
import json

HOST = '127.0.0.1'
PORT = 50007

WINDOW_SIZE = 800
GRID_SIZE = 100
CELL_SIZE = 10

class GameOfLifeGUI:
    def __init__(self):
        self.root = tk.Tk()
        self.root.title("Game of Life")
        self.canvas = Canvas(self.root, width=WINDOW_SIZE, height=WINDOW_SIZE, bg="white")
        self.canvas.pack(fill=tk.BOTH, expand=True)
        self.draw_grid()

        self.cells = set()
        self.rects = {}
        self.lock = threading.Lock()
        
        # Mouse bindings
        self.canvas.bind("<Button-1>", self.left_click)   # single-click toggle
        self.canvas.bind("<B1-Motion>", self.left_drag)   # left mouse drag for painting
        self.canvas.bind("<ButtonPress-3>", self.right_click_press)
        self.canvas.bind("<B3-Motion>", self.right_click_drag)
        self.canvas.bind("<ButtonRelease-3>", self.right_click_release)

    def set_server(self, server):
        self.server = server

    def draw_grid(self):
        for i in range(GRID_SIZE + 1):
            self.canvas.create_line(i * CELL_SIZE, 0, i * CELL_SIZE, GRID_SIZE * CELL_SIZE, fill="#ddd")
            self.canvas.create_line(0, i * CELL_SIZE, GRID_SIZE * CELL_SIZE, i * CELL_SIZE, fill="#ddd")

    def start(self):
        self.root.mainloop()

    def left_click(self, event):
        x = self.canvas.canvasx(event.x)
        y = self.canvas.canvasy(event.y)
        cell_x = int(x // CELL_SIZE)
        cell_y = int(y // CELL_SIZE)
        with self.lock:
            if (cell_x, cell_y) in self.cells:
                # black -> empty
                self.cells.remove((cell_x, cell_y))
                if (cell_x, cell_y) in self.rects:
                    self.canvas.delete(self.rects[(cell_x, cell_y)])
                    del self.rects[(cell_x, cell_y)]
            else:
                # empty -> black
                self.cells.add((cell_x, cell_y))
                rect = self.canvas.create_rectangle(
                    cell_x * CELL_SIZE,
                    cell_y * CELL_SIZE,
                    (cell_x + 1) * CELL_SIZE,
                    (cell_y + 1) * CELL_SIZE,
                    fill="black",
                    tags="cell"
                )
                self.rects[(cell_x, cell_y)] = rect
        self.server.broadcast_cells()

    def left_drag(self, event):
        x = self.canvas.canvasx(event.x)
        y = self.canvas.canvasy(event.y)
        cell_x = int(x // CELL_SIZE)
        cell_y = int(y // CELL_SIZE)
        with self.lock:
            if (cell_x, cell_y) not in self.cells:
                self.cells.add((cell_x, cell_y))
                rect = self.canvas.create_rectangle(
                    cell_x * CELL_SIZE,
                    cell_y * CELL_SIZE,
                    (cell_x + 1) * CELL_SIZE,
                    (cell_y + 1) * CELL_SIZE,
                    fill="black",
                    tags="cell"
                )
                self.rects[(cell_x, cell_y)] = rect
        self.server.broadcast_cells()
    
    def right_click_press(self, event):
        self.canvas.scan_mark(event.x, event.y)

    def right_click_drag(self, event):
        self.canvas.scan_dragto(event.x, event.y, gain=1)

    def right_click_release(self, event):
        pass

    def get_initial_cells(self):
        with self.lock:
            return list(self.cells)

class GameOfLifeServer:
    def __init__(self, gui):
        self.gui = gui
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server.bind((HOST, PORT))
        self.server.listen(5)
        print(f"[INFO] Server started on {HOST}:{PORT}")
        self.clients = []
        self.lock = threading.Lock()

    def handle_client(self, client_socket, addr):
        print(f"[INFO] New connection from {addr}")
        try:
            initial_cells = self.gui.get_initial_cells()
            initial_data = json.dumps({"cells": initial_cells}) + "\n"
            print(f"[DEBUG] Sending initial cells to {addr}: {initial_data}")
            client_socket.sendall(initial_data.encode('utf-8'))

            while True:
                data = client_socket.recv(1024)
                if not data:
                    break
        except Exception as e:
            print(f"[ERROR] Connection with {addr} lost: {e}")
        finally:
            with self.lock:
                if client_socket in self.clients:
                    self.clients.remove(client_socket)
            client_socket.close()
            print(f"[INFO] Connection with {addr} closed.")

    def broadcast_cells(self):
        with self.lock:
            cells = self.gui.get_initial_cells()
            data = json.dumps({"cells": cells}) + "\n"
            for client in self.clients[:]:
                try:
                    client.sendall(data.encode('utf-8'))
                except Exception as e:
                    print(f"[ERROR] Failed to send data to {client.getpeername()}: {e}")
                    self.clients.remove(client)
                    client.close()

    def run(self):
        while True:
            client, addr = self.server.accept()
            print(f"[DEBUG] Accepted new client: {addr}")
            with self.lock:
                self.clients.append(client)
            thread = threading.Thread(target=self.handle_client, args=(client, addr))
            thread.daemon = True
            thread.start()

def main():
    gui = GameOfLifeGUI()
    server = GameOfLifeServer(gui)
    gui.set_server(server)  # Link GUI with server
    server_thread = threading.Thread(target=server.run)
    server_thread.daemon = True
    server_thread.start()
    gui.start()

if __name__ == "__main__":
    main()
