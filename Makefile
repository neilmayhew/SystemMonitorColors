SHELL := nix-shell
.SHELLFLAGS := --run

PROG := SystemMonitorColors.hs

.PHONY: check set stress clean

check:
	runghc $(PROG) --unit

set:
	dconf write /org/gnome/gnome-system-monitor/cpu-colors "$$(runghc $(PROG) -d)"

stress:
	stress -c 16

test-colors.html: $(PROG)
	runghc $< --html >$@

clean:
	$(RM) test-colors.html
