import socket

HOST = '127.0.0.1'
PORT = 50007

def start_server():
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as server_socket:
        server_socket.bind((HOST, PORT))
        server_socket.listen()
        print(f"Server listening on {HOST}:{PORT}...")
        conn, addr = server_socket.accept()
        with conn:
            print(f"Connected by {addr}")
            message = "Hello from Python server!"
            conn.sendall(message.encode('utf-8'))
            print("Message sent to client")

if __name__ == "__main__":
    start_server()
