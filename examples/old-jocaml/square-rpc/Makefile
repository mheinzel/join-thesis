CC=joc


build: square-server.out square-client.out


run-server: square-server.out
	gnome-terminal -x bash -c "jocns || sleep 10"
	sleep 5
	gnome-terminal -x bash -c "./square-server.out || sleep 10"

run-client: square-client.out
	./square-client.out


square-server.out: square-server.ml
	$(CC) square-server.ml -o square-server.out
	rm square-server.cmi square-server.cmo

square-client.out: square-client.ml
	$(CC) square-client.ml -o square-client.out
	rm square-client.cmi square-client.cmo


clean:
	rm -f *.cmo *.cmi *.out
