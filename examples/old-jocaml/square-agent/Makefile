CC=joc


build: square-location.out square-agent.out


run-location: square-location.out
	gnome-terminal -x bash -c "jocns || sleep 10"
	sleep 5
	gnome-terminal -x bash -c "./square-location.out || sleep 10"

run-agent: square-agent.out
	./square-agent.out


square-location.out: square-location.ml
	$(CC) square-location.ml -o square-location.out
	rm square-location.cmi square-location.cmo

square-agent.out: square-agent.ml
	$(CC) square-agent.ml -o square-agent.out
	rm square-agent.cmi square-agent.cmo


clean:
	rm -f *.cmo *.cmi *.out
