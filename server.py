import socket
import threading
import tkinter as tk
from tkinter import Canvas

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
        
        #Mouse bindings
        self.canvas.bind("<Button-1>", self.left_click)   # single-click toggle
        self.canvas.bind("<B1-Motion>", self.left_drag)   # left mouse drag for painting

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


class GameOfLifeServer:
    def __init__(self):
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server.bind((HOST, PORT))
        self.server.listen(5)
        print(f"[INFO] Server started on {HOST}:{PORT}")

    def handle_client(self, client_socket, addr):
        print(f"[INFO] Connected by {addr}")
        client_socket.sendall(b"Hello from server!")
        while True:
            data = client_socket.recv(1024)
            if not data:
                break
        client_socket.close()

    def run(self):
        while True:
            client_socket, addr = self.server.accept()
            thread = threading.Thread(target=self.handle_client, args=(client_socket, addr), daemon=True)
            thread.start()

def main():
    gui = GameOfLifeGUI()
    server = GameOfLifeServer()
    gui.set_server(server)
    server_thread = threading.Thread(target=server.run, daemon=True)
    server_thread.start()
    gui.start()

if __name__ == "__main__":
    main()
