CC=joc


build: cell-server.out cell-client.out


run-server: cell-server.out
	gnome-terminal -x bash -c "jocns || sleep 10"
	sleep 5
	gnome-terminal -x bash -c "./cell-server.out || sleep 10"

run-client: cell-client.out
	./cell-client.out


cell-server.out: cell-server.ml
	$(CC) cell-server.ml -o cell-server.out
	rm cell-server.cmi cell-server.cmo

cell-client.out: cell-client.ml
	$(CC) cell-client.ml -o cell-client.out
	rm cell-client.cmi cell-client.cmo


clean:
	rm -f *.cmo *.cmi *.out
