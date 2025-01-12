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
        self.canvas = Canvas(self.root, width=WINDOW_SIZE, height=WINDOW_SIZE, bg="white", scrollregion=(0, 0, GRID_SIZE * CELL_SIZE, GRID_SIZE * CELL_SIZE))
        self.canvas.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        self.draw_grid()

        self.cells = set()
        self.rects = {}

        # Mouse bindings
        self.canvas.bind("<Button-1>", self.left_click)        
        self.canvas.bind("<B1-Motion>", self.left_drag)        
        self.canvas.bind("<ButtonPress-3>", self.right_click_press)
        self.canvas.bind("<B3-Motion>", self.right_click_drag)
        self.canvas.bind("<ButtonRelease-3>", self.right_click_release)

        self.lock = threading.Lock()

    def draw_grid(self):
        for i in range(GRID_SIZE + 1):
            self.canvas.create_line(i * CELL_SIZE, 0, i * CELL_SIZE, GRID_SIZE * CELL_SIZE, fill="#ddd", tags="grid")
            self.canvas.create_line(0, i * CELL_SIZE, GRID_SIZE * CELL_SIZE, i * CELL_SIZE, fill="#ddd", tags="grid")

    def left_click(self, event):
        x = self.canvas.canvasx(event.x)
        y = self.canvas.canvasy(event.y)
        cell_x = int(x // CELL_SIZE)
        cell_y = int(y // CELL_SIZE)
        
        with self.lock:
            if (cell_x, cell_y) in self.cells:
                self.cells.remove((cell_x, cell_y))
                if (cell_x, cell_y) in self.rects:
                    self.canvas.delete(self.rects[(cell_x, cell_y)])
                    del self.rects[(cell_x, cell_y)]
            else:
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

    def right_click_press(self, event):
        self.canvas.scan_mark(event.x, event.y)

    def right_click_drag(self, event):
        self.canvas.scan_dragto(event.x, event.y, gain=1)

    def right_click_release(self, event):
        pass

    def draw_cell(self, cell):
        x, y = cell
        if cell not in self.cells:
            self.cells.add(cell)
            rect = self.canvas.create_rectangle(
                x * CELL_SIZE,
                y * CELL_SIZE,
                (x + 1) * CELL_SIZE,
                (y + 1) * CELL_SIZE,
                fill="black",
                tags="cell"
            )
            self.rects[cell] = rect

    def delete_cell(self, cell):
        if cell in self.cells:
            self.cells.remove(cell)
            if cell in self.rects:
                self.canvas.delete(self.rects[cell])
                del self.rects[cell]

    def update_cells(self, new_cells):
        print(f"[DEBUG] update_cells() called with {len(new_cells)} new cells.")
        with self.lock:
            cells_to_add = new_cells - self.cells
            for c in cells_to_add:
                print(f"[DEBUG] Adding cell: {c}")
                self.draw_cell(c)
            self.cells.update(cells_to_add)
        print("[DEBUG] update_cells() finished adding new cells to GUI.")

    def get_initial_cells(self):
        with self.lock:
            return list(self.cells)

    def start(self):
        self.root.mainloop()


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
            initial_data = json.dumps({"cells": initial_cells})
            print(f"[DEBUG] Sending initial cells to {addr}: {initial_data}")
            client_socket.sendall((initial_data + "\n").encode('utf-8'))

            while True:
                data = client_socket.recv(4096)
                if not data:
                    print(f"[DEBUG] No data from {addr}. Closing.")
                    break
                messages = data.decode('utf-8').split('\n')
                for message in messages:
                    if not message.strip():
                        continue
                    print(f"[DEBUG] Received from {addr}: {message}")

                    try:
                        command = json.loads(message)
                        cmd_type = command.get("cmd")

                        if cmd_type == "UPDATE":
                            new_cells = set(tuple(c) for c in command.get("cells", []))
                            print(f"[DEBUG] Received 'UPDATE' from {addr}. New cells to add: {new_cells}")

                            self.gui.update_cells(new_cells)
                            
                            updated_data = json.dumps({"cells": list(map(list, self.gui.get_initial_cells()))})
                            client_socket.sendall((updated_data + "\n").encode('utf-8'))
                            print(f"[DEBUG] Sent updated cell set to {addr}: {updated_data}")
                        else:
                            print(f"[WARN] Unknown command from {addr}: {cmd_type}")

                    except json.JSONDecodeError:
                        print(f"[ERROR] Failed to parse JSON from {addr}: {message}")

        except ConnectionResetError:
            print(f"[WARN] Connection reset by {addr}")
        finally:
            client_socket.close()
            with self.lock:
                if client_socket in self.clients:
                    self.clients.remove(client_socket)
            print(f"[INFO] Connection closed from {addr}")

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
    server_thread = threading.Thread(target=server.run)
    server_thread.daemon = True
    server_thread.start()
    gui.start()

if __name__ == "__main__":
    main()
