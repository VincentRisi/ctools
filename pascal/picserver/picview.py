import socket
import sys

HOST, PORT = "localhost", 32145
data = '1005\0x00' # sys.argv[1]

# Create a socket (SOCK_STREAM means a TCP socket)
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

try:
    # Connect to server and send data
    sock.connect((HOST, PORT))
    sock.sendall(data + "\n")

    # Receive data from the server and shut down
    received = sock.recv(1024)
    print "Sent:     {}".format(data)
    print "Received: {}".format(received)
except:
    print "Failed"
finally:
    sock.close()

